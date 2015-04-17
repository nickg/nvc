//
//  Copyright (C) 2011-2015  Nick Gasson
//
//  This program is free software: you can redistribute it and/or modify
//  it under the terms of the GNU General Public License as published by
//  the Free Software Foundation, either version 3 of the License, or
//  (at your option) any later version.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//  GNU General Public License for more details.
//
//  You should have received a copy of the GNU General Public License
//  along with this program.  If not, see <http://www.gnu.org/licenses/>.
//

#include "phase.h"
#include "util.h"
#include "common.h"
#include "hash.h"

#include <assert.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>
#include <ctype.h>
#include <errno.h>
#include <sys/stat.h>
#include <sys/types.h>

typedef struct scope       scope_t;
typedef struct loop_stack  loop_stack_t;
typedef struct type_set    type_set_t;
typedef struct defer_check defer_check_t;
typedef struct import_list import_list_t;

typedef bool (*defer_fn_t)(tree_t t);

struct defer_check {
   defer_check_t *next;
   defer_fn_t     fn;
   tree_t         arg;
};

struct import_list {
   ident_t        name;
   bool           all;
   import_list_t *next;
};

typedef enum {
   SCOPE_PACKAGE   = (1 << 0),
   SCOPE_FORMAL    = (1 << 1),
   SCOPE_PROTECTED = (1 << 2)
} scope_flags_t;

struct scope {
   scope_t       *down;

   defer_check_t *deferred;
   hash_t        *decls;
   tree_t         subprog;

   // For design unit scopes
   ident_t        prefix;
   import_list_t *imported;
   scope_flags_t  flags;
};

struct loop_stack {
   loop_stack_t *up;
   ident_t       name;
};

struct type_set {
   type_t     *members;
   unsigned    n_members;
   unsigned    alloc;
   type_set_t *down;
};

typedef struct {
   const loc_t *loc;
   lib_t        lib;
   bool         error;
} lib_walk_params_t;

typedef struct {
   tree_t decl;
   bool   have;
   bool   partial;
} formal_map_t;

typedef tree_t (*get_fn_t)(tree_t);
typedef void (*set_fn_t)(tree_t, tree_t);

static bool sem_check_constrained(tree_t t, type_t type);
static bool sem_check_array_ref(tree_t t);
static bool sem_declare(tree_t decl, bool add_predefined);
static bool sem_locally_static(tree_t t);
static bool sem_globally_static(tree_t t);
static tree_t sem_check_lvalue(tree_t t);
static bool sem_check_type(tree_t t, type_t *ptype);
static bool sem_static_name(tree_t t);
static bool sem_check_range(range_t *r, type_t context);
static void sem_add_attributes(tree_t decl, bool is_signal);
static type_t sem_implicit_dereference(tree_t t, get_fn_t get, set_fn_t set);

static scope_t      *top_scope = NULL;
static int           errors = 0;
static unsigned      relax = 0;
static type_set_t   *top_type_set = NULL;
static loop_stack_t *loop_stack = NULL;
static ident_t       builtin_i;
static ident_t       std_standard_i;
static ident_t       formal_i;
static ident_t       locally_static_i;
static ident_t       elab_copy_i;
static ident_t       all_i;
static ident_t       shared_i;
static ident_t       unconstrained_i;
static ident_t       protected_i;
static ident_t       impure_i;

#define sem_error(t, ...) do {                        \
      error_at(t ? tree_loc(t) : NULL , __VA_ARGS__); \
      errors++;                                       \
      return false;                                   \
   } while (0)

static void scope_push(ident_t prefix)
{
   scope_t *s = xmalloc(sizeof(scope_t));
   s->decls    = hash_new(1024, false);
   s->prefix   = prefix;
   s->imported = NULL;
   s->down     = top_scope;
   s->subprog  = (top_scope ? top_scope->subprog : NULL) ;
   s->flags    = (top_scope ? top_scope->flags : 0);
   s->deferred = NULL;

   top_scope = s;
}

static void scope_pop(void)
{
   assert(top_scope != NULL);
   assert(top_scope->deferred == NULL);

   while (top_scope->imported != NULL) {
      import_list_t *tmp = top_scope->imported->next;
      free(top_scope->imported);
      top_scope->imported = tmp;
   }

   hash_free(top_scope->decls);

   scope_t *s = top_scope;
   top_scope = s->down;
   free(s);
}

static void scope_defer_check(defer_fn_t fn, tree_t arg)
{
   assert(top_scope != NULL);

   defer_check_t *d = xmalloc(sizeof(defer_check_t));
   d->next = NULL;
   d->fn   = fn;
   d->arg  = arg;

   if (top_scope->deferred == NULL)
      top_scope->deferred = d;
   else {
      defer_check_t *where = top_scope->deferred;
      while (where->next != NULL)
         where = where->next;
      where->next = d;
   }
}

static bool scope_run_deferred_checks(void)
{
   assert(top_scope != NULL);

   bool result = true;
   defer_check_t *it = top_scope->deferred;
   while (it != NULL) {
      defer_check_t *tmp = it->next;
      result = (*it->fn)(it->arg) && result;
      free(it);
      it = tmp;
   }

   top_scope->deferred = NULL;

   return result;
}

static void scope_apply_prefix(tree_t t)
{
   if (top_scope->prefix)
      tree_set_ident(t, ident_prefix(top_scope->prefix,
                                     tree_ident(t), '.'));
}

static scope_t *scope_containing(scope_t *s, tree_t decl)
{
   int k = 0, tmp;
   tree_t next;
   ident_t name = tree_ident(decl);
   while (tmp = k++, (next = hash_get_nth(s->decls, name, &tmp))) {
      if (next == decl)
         return s;
   }
   return s->down == NULL ? NULL : scope_containing(s->down, decl);
}

static tree_t scope_find_in(ident_t i, scope_t *s, bool recur, int k)
{
   if (s == NULL)
      return NULL;
   else {
      void *value = hash_get_nth(s->decls, i, &k);
      if (value != NULL)
         return (tree_t)value;
      else
         return (recur ? scope_find_in(i, s->down, true, k) : NULL);
   }
}

static tree_t scope_find(ident_t i)
{
   return scope_find_in(i, top_scope, true, 0);
}

static tree_t scope_find_nth(ident_t i, int n)
{
   return scope_find_in(i, top_scope, true, n);
}

static bool scope_walk(hash_iter_t *now, tree_t *decl)
{
   const void *key;
   void *value;
   while (hash_iter(top_scope->decls, now, &key, &value)) {
      *decl = value;
      if (tree_ident(*decl) != key)
         continue;   // Skip aliases
      else
         return true;
   }

   return false;
}

static bool scope_can_overload(tree_t t)
{
   tree_kind_t kind = tree_kind(t);
   return (kind == T_ENUM_LIT)
      || (kind == T_FUNC_DECL)
      || (kind == T_FUNC_BODY)
      || (kind == T_PROC_DECL)
      || (kind == T_PROC_BODY);
}

static bool scope_insert_hiding(tree_t t, ident_t name, bool overload)
{
   assert(top_scope != NULL);

   tree_t existing;
   int n = 0;
   do {
      if ((existing = scope_find_in(name, top_scope, false, n++))) {
         if (!overload)
            sem_error(t, "%s already declared in this region", istr(name));

         if (tree_kind(existing) == T_UNIT_DECL)
            continue;

         const bool builtin = (tree_attr_str(existing, builtin_i) != NULL);
         if (builtin && type_eq(tree_type(t), tree_type(existing))) {
            type_t arg0_type = type_param(tree_type(existing), 0);

            ident_t t_region = ident_runtil(tree_ident(t), '.');
            ident_t e_region = ident_runtil(type_ident(arg0_type), '.');

            const bool same_region = (t_region == e_region);

            // Allow builtin functions to be hidden by explicit functins
            // declared in the same region
            if (same_region) {
               hash_replace(top_scope->decls, existing, t);
               return true;
            }
         }
      }
   } while (existing != NULL);

   hash_put(top_scope->decls, name, t);
   return true;
}

static bool scope_insert(tree_t t)
{
   return scope_insert_hiding(t, tree_ident(t), scope_can_overload(t));
}

static void scope_insert_alias(tree_t t, ident_t name)
{
   (void)scope_insert_hiding(t, name, true);
}

static void scope_replace(tree_t t, tree_t with)
{
   assert(top_scope != NULL);
   hash_replace(top_scope->decls, t, with);
}

static void loop_push(ident_t name)
{
   loop_stack_t *ls = xmalloc(sizeof(loop_stack_t));
   ls->up   = loop_stack;
   ls->name = name;

   loop_stack = ls;
}

static void loop_pop(void)
{
   loop_stack_t *tmp = loop_stack->up;
   free(loop_stack);
   loop_stack = tmp;
}

static const char *sem_type_minify(const char *name)
{
   // If name without its fully qualified prefix is unique in the
   // current scope then drop the prefix to make messages easier
   // to read.
   // E.g. IEEE.STD_LOGIC_1164.STD_LOGIC -> STD_LOGIC if no other
   //      STD_LOGIC type is visible

   const char *suffix = strrchr(name, '.');
   if (suffix == NULL)
      return name;

   ident_t suffix_i = ident_new(suffix + 1);

   int matches = 0;
   for (scope_t *s = top_scope; s != NULL; s = s->down) {
      for (import_list_t *i = s->imported; i != NULL; i = i->next) {
         ident_t search = ident_prefix(i->name, suffix_i, '.');
         if (scope_find_in(search, s, false, 0) != NULL)
            matches++;
      }
   }

   return (matches == 1) ? suffix + 1 : name;
}

static const char *sem_type_str(type_t type)
{
   return type_pp_minify(type, sem_type_minify);
}

static bool scope_import_unit(ident_t unit_name, lib_t lib,
                              bool all, const loc_t *loc)
{
   // Check we haven't already imported this
   bool unqual_only = false;
   for (scope_t *s = top_scope; s != NULL; s = s->down) {
      import_list_t *it;
      for (it = s->imported; it != NULL; it = it->next) {
         if (it->name == unit_name) {
            if (it->all || !all)
               return true;
            else {
               unqual_only = true;
               break;
            }
         }
      }
   }

   tree_t unit = lib_get_check_stale(lib, unit_name);
   if (unit == NULL) {
      error_at(loc, "unit %s not found in library %s",
               istr(unit_name), istr(lib_name(lib)));
      ++errors;
      return false;
   }

   const int ndecls = tree_decls(unit);
   for (int n = 0; n < ndecls; n++) {
      tree_t decl = tree_decl(unit, n);

      tree_kind_t kind = tree_kind(decl);
      if ((kind == T_ATTR_SPEC) || (kind == T_USE))
         continue;

      ident_t dname = tree_ident(decl);

      // Make unqualified and package qualified names visible
      if (!unqual_only) {
         if (!sem_declare(decl, true))
            return false;

         ident_t pqual = ident_from(dname, '.');
         if (pqual != NULL)
            scope_insert_alias(decl, pqual);
      }

      if (all) {
         ident_t unqual = ident_rfrom(dname, '.');
         if (unqual != NULL)
            scope_insert_alias(decl, unqual);
      }
   }

   import_list_t *new = xmalloc(sizeof(import_list_t));
   new->name = unit_name;
   new->all  = all;
   new->next = top_scope->imported;

   top_scope->imported = new;
   return true;
}

static void type_set_push(void)
{
   type_set_t *t = xmalloc(sizeof(type_set_t));
   t->n_members = 0;
   t->alloc     = 32;
   t->members   = xmalloc(t->alloc * sizeof(type_t));
   t->down      = top_type_set;

   top_type_set = t;
}

static void type_set_pop(void)
{
   assert(top_type_set != NULL);

   type_set_t *old = top_type_set;
   top_type_set = old->down;
   free(old->members);
   free(old);
}

static void type_set_add(type_t t)
{
   assert(top_type_set != NULL);
   assert(t != NULL);
   assert(type_kind(t) != T_UNRESOLVED);

   for (unsigned i = 0; i < top_type_set->n_members; i++) {
      if (type_eq(top_type_set->members[i], t))
         return;
   }

   ARRAY_APPEND(top_type_set->members, t, top_type_set->n_members,
                top_type_set->alloc);
}

static bool type_set_restrict(bool (*pred)(type_t))
{
   if (top_type_set == NULL)
      return false;

   int j = 0;
   for (int i = 0; i < top_type_set->n_members; i++) {
      type_t type = top_type_set->members[i];
      if ((*pred)(type))
         top_type_set->members[j++] = type;
   }
   top_type_set->n_members = j;

   return j > 0;
}

static bool type_set_uniq(type_t *pt)
{
   assert(top_type_set != NULL);

   if (top_type_set->n_members == 1) {
      *pt = top_type_set->members[0];
      return true;
   }
   else {
      *pt = NULL;
      return false;
   }
}

static bool type_set_any(type_t *pt)
{
   if ((top_type_set == NULL) || (top_type_set->n_members == 0))
      return false;
   else {
      *pt = top_type_set->members[0];
      return true;
   }
}

#if 0
static void type_set_dump(void)
{
   printf("type_set: { ");
   if (top_type_set) {
      for (unsigned n = 0; n < top_type_set->n_members; n++)
         printf("%s ", sem_type_str(top_type_set->members[n]));
   }
   printf("}\n");
}
#endif

static text_buf_t *type_set_fmt(void)
{
   text_buf_t *tb = tb_new();

   if (top_type_set != NULL) {
      for (unsigned n = 0; n < top_type_set->n_members; n++)
         tb_printf(tb, "\n    %s",
                   sem_type_str(top_type_set->members[n]));
   }

   return tb;
}

static bool type_set_member(type_t t)
{
   if (top_type_set == NULL || top_type_set->n_members == 0)
      return true;

   for (unsigned n = 0; n < top_type_set->n_members; n++) {
      if (type_eq(top_type_set->members[n], t))
         return true;
   }

   return false;
}

static type_t sem_std_type(const char *name)
{
   ident_t name_i = ident_new(name);
   ident_t qual = ident_prefix(std_standard_i, name_i, '.');
   tree_t decl = scope_find(qual);
   if (decl == NULL)
      fatal("cannot find %s type", istr(qual));

   return tree_type(decl);
}

static tree_t sem_add_port(tree_t d, type_t type, port_mode_t mode, tree_t def)
{
   type_t ftype = tree_type(d);

   char *argname LOCAL = xasprintf("_arg%d", type_params(ftype));
   tree_t port = tree_new(T_PORT_DECL);
   tree_set_ident(port, ident_new(argname));
   tree_set_type(port, type);
   tree_set_subkind(port, mode);
   if (def != NULL)
      tree_set_value(port, def);

   tree_add_port(d, port);
   type_add_param(ftype, type);

   return port;
}

static tree_t sem_builtin_proc(ident_t name, const char *builtin, ...)
{
   type_t f = type_new(T_PROC);
   type_set_ident(f, name);

   tree_t d = tree_new(T_PROC_DECL);
   tree_set_ident(d, name);
   tree_set_type(d, f);
   tree_add_attr_str(d, builtin_i, ident_new(builtin));

   return d;
}

static tree_t sem_builtin_fn(ident_t name, type_t result,
                             const char *builtin, ...)
{
   type_t f = type_new(T_FUNC);
   type_set_ident(f, name);
   type_set_result(f, result);

   tree_t d = tree_new(T_FUNC_DECL);
   tree_set_ident(d, name);
   tree_set_type(d, f);
   tree_add_attr_str(d, builtin_i, ident_new(builtin));

   va_list ap;
   va_start(ap, builtin);
   type_t arg;
   while ((arg = va_arg(ap, type_t)))
      sem_add_port(d, arg, PORT_IN, NULL);
   va_end(ap);

   return d;
}

static void sem_declare_binary(tree_t decl, ident_t name, type_t lhs,
                               type_t rhs, type_t result, const char *builtin)
{
   tree_t d = sem_builtin_fn(name, result, builtin, lhs, rhs, NULL);
   scope_insert(d);

   if (decl != NULL)
      tree_add_op(decl, d);
}

static void sem_declare_unary(tree_t decl, ident_t name, type_t operand,
                              type_t result, const char *builtin)
{
   tree_t d = sem_builtin_fn(name, result, builtin, operand, NULL);
   scope_insert(d);

   if (decl != NULL)
      tree_add_op(decl, d);
}

static tree_t sem_bool_lit(type_t std_bool, bool v)
{
   tree_t lit = type_enum_literal(std_bool, v ? 1 : 0);
   return make_ref(lit);
}

static tree_t sem_int_lit(type_t type, int64_t i)
{
   tree_t f = tree_new(T_LITERAL);
   tree_set_subkind(f, L_INT);
   tree_set_ival(f, i);
   tree_set_type(f, type);

   return f;
}

static void sem_add_dimension_attr(tree_t decl, type_t rtype, const char *name,
                                   const char *builtin)
{
   if (rtype == NULL) {
      type_t dtype = tree_type(decl);
      if (type_kind(dtype) == T_ACCESS)
         dtype = type_access(dtype);

      if (type_is_array(dtype))
         rtype = (array_dimension(dtype) > 0)
            ? type_new(T_NONE)
            : index_type_of(dtype, 0);
      else
         rtype = dtype;
   }

   ident_t length_i = ident_new(name);
   type_t std_int = sem_std_type("INTEGER");
   tree_t fn = sem_builtin_fn(length_i, rtype, builtin,
                              std_int, tree_type(decl), NULL);

   // Dimension argument defaults to 1
   tree_t dim = tree_port(fn, 0);
   tree_set_value(dim, sem_int_lit(std_int, 1));
   tree_add_attr_int(dim, locally_static_i, 1);

   tree_add_attr_tree(decl, length_i, fn);
}

static void sem_declare_predefined_ops(tree_t decl)
{
   // Prefined operators are defined in LRM 93 section 7.2

   type_t t = tree_type(decl);

   ident_t mult  = ident_new("\"*\"");
   ident_t div   = ident_new("\"/\"");
   ident_t plus  = ident_new("\"+\"");
   ident_t minus = ident_new("\"-\"");

   // Predefined operators

   type_t std_bool   = sem_std_type("BOOLEAN");
   type_t std_int    = sem_std_type("INTEGER");
   type_t std_real   = sem_std_type("REAL");
   type_t std_string = sem_std_type("STRING");

   type_kind_t kind = type_kind(t);
   assert(kind != T_UNRESOLVED);

   switch (kind) {
   case T_SUBTYPE:
      // Use operators of base type
      break;

   case T_CARRAY:
   case T_UARRAY:
      // Operators on arrays
      sem_declare_binary(decl, ident_new("\"=\""), t, t, std_bool, "aeq");
      sem_declare_binary(decl, ident_new("\"/=\""), t, t, std_bool, "aneq");
      if (array_dimension(t) == 1) {
         sem_declare_binary(decl, ident_new("\"<\""), t, t, std_bool, "alt");
         sem_declare_binary(decl, ident_new("\"<=\""), t, t, std_bool, "aleq");
         sem_declare_binary(decl, ident_new("\">\""), t, t, std_bool, "agt");
         sem_declare_binary(decl, ident_new("\">=\""), t, t, std_bool, "ageq");
      }
      break;

   case T_RECORD:
      // Operators on records
      sem_declare_binary(decl, ident_new("\"=\""), t, t, std_bool, "req");
      sem_declare_binary(decl, ident_new("\"/=\""), t, t, std_bool, "rneq");
      break;

   case T_PHYSICAL:
      // Multiplication
      sem_declare_binary(decl, mult, t, std_int, t, "mul");
      sem_declare_binary(decl, mult, t, std_real, t, "mulpr");
      sem_declare_binary(decl, mult, std_int, t, t, "mul");
      sem_declare_binary(decl, mult, std_real, t, t, "mulrp");

      // Division
      sem_declare_binary(decl, div, t, std_int, t, "div");
      sem_declare_binary(decl, div, t, std_real, t, "divpr");
      sem_declare_binary(decl, div, t, t, std_int, "div");

      // Addition
      sem_declare_binary(decl, plus, t, t, t, "add");

      // Subtraction
      sem_declare_binary(decl, minus, t, t, t, "sub");

      // Sign operators
      sem_declare_unary(decl, plus, t, t, "identity");
      sem_declare_unary(decl, minus, t, t, "neg");

      // Comparison
      sem_declare_binary(decl, ident_new("\"<\""), t, t, std_bool, "lt");
      sem_declare_binary(decl, ident_new("\"<=\""), t, t, std_bool, "leq");
      sem_declare_binary(decl, ident_new("\">\""), t, t, std_bool, "gt");
      sem_declare_binary(decl, ident_new("\">=\""), t, t, std_bool, "geq");

      // Equality
      sem_declare_binary(decl, ident_new("\"=\""), t, t, std_bool, "eq");
      sem_declare_binary(decl, ident_new("\"/=\""), t, t, std_bool, "neq");

      // Absolute value
      sem_declare_unary(decl, ident_new("\"abs\""), t, t, "abs");

      break;

   case T_INTEGER:
      // Modulus
      sem_declare_binary(decl, ident_new("\"mod\""), t, t, t, "mod");

      // Remainder
      sem_declare_binary(decl, ident_new("\"rem\""), t, t, t, "rem");

      // Fall-through
   case T_REAL:
      // Addition
      sem_declare_binary(decl, plus, t, t, t, "add");

      // Subtraction
      sem_declare_binary(decl, minus, t, t, t, "sub");

      // Multiplication
      sem_declare_binary(decl, mult, t, t, t, "mul");

      // Division
      sem_declare_binary(decl, div, t, t, t, "div");

      // Sign operators
      sem_declare_unary(decl, plus, t, t, "identity");
      sem_declare_unary(decl, minus, t, t, "neg");

      // Exponentiation
      sem_declare_binary(decl, ident_new("\"**\""), t, std_int, t, "exp");

      // Absolute value
      sem_declare_unary(decl, ident_new("\"abs\""), t, t, "abs");

      // Fall-through
   case T_ENUM:
      sem_declare_binary(decl, ident_new("\"<\""), t, t, std_bool, "lt");
      sem_declare_binary(decl, ident_new("\"<=\""), t, t, std_bool, "leq");
      sem_declare_binary(decl, ident_new("\">\""), t, t, std_bool, "gt");
      sem_declare_binary(decl, ident_new("\">=\""), t, t, std_bool, "geq");

      // Fall-through
   default:
      sem_declare_binary(decl, ident_new("\"=\""), t, t, std_bool, "eq");
      sem_declare_binary(decl, ident_new("\"/=\""), t, t, std_bool, "neq");

      break;
   }

   // Logical operators

   ident_t boolean_i = ident_new("STD.STANDARD.BOOLEAN");
   ident_t bit_i = ident_new("STD.STANDARD.BIT");

   bool logical = (type_ident(t) == boolean_i || type_ident(t) == bit_i);

   if (logical) {
      sem_declare_binary(decl, ident_new("\"and\""), t, t, t, "and");
      sem_declare_binary(decl, ident_new("\"or\""), t, t, t, "or");
      sem_declare_binary(decl, ident_new("\"xor\""), t, t, t, "xor");
      sem_declare_binary(decl, ident_new("\"nand\""), t, t, t, "nand");
      sem_declare_binary(decl, ident_new("\"nor\""), t, t, t, "nor");
      sem_declare_binary(decl, ident_new("\"xnor\""), t, t, t, "xnor");
      sem_declare_unary(decl, ident_new("\"not\""), t, t, "not");
   }

   bool vec_logical = false;
   if (kind == T_CARRAY || kind == T_UARRAY) {
      type_t base = type_elem(t);
      vec_logical = (type_ident(base) == boolean_i
                     || type_ident(base) == bit_i);
   }

   if (vec_logical) {
      sem_declare_binary(decl, ident_new("\"and\""), t, t, t, "v_and");
      sem_declare_binary(decl, ident_new("\"or\""), t, t, t, "v_or");
      sem_declare_binary(decl, ident_new("\"xor\""), t, t, t, "v_xor");
      sem_declare_binary(decl, ident_new("\"nand\""), t, t, t, "v_nand");
      sem_declare_binary(decl, ident_new("\"nor\""), t, t, t, "v_nor");
      sem_declare_binary(decl, ident_new("\"xnor\""), t, t, t, "v_xnor");
      sem_declare_unary(decl, ident_new("\"not\""), t, t, "v_not");

      sem_declare_binary(decl, ident_new("\"sll\""), t, std_int, t, "sll");
      sem_declare_binary(decl, ident_new("\"srl\""), t, std_int, t, "srl");
      sem_declare_binary(decl, ident_new("\"sla\""), t, std_int, t, "sla");
      sem_declare_binary(decl, ident_new("\"sra\""), t, std_int, t, "sra");
      sem_declare_binary(decl, ident_new("\"rol\""), t, std_int, t, "rol");
      sem_declare_binary(decl, ident_new("\"ror\""), t, std_int, t, "ror");
   }

   // Predefined procedures

   switch (kind) {
   case T_FILE:
      {
         tree_t read_mode = scope_find(ident_new("READ_MODE"));
         assert(read_mode != NULL);

         // The underlying type of the file may not have been checked yet
         type_t of = type_file(t);
         tree_t type_decl = scope_find(type_ident(of));
         if (type_decl == NULL)
            break;    // Will generate error later
         of = tree_type(type_decl);

         ident_t file_open_i  = ident_new("FILE_OPEN");
         ident_t file_close_i = ident_new("FILE_CLOSE");
         ident_t read_i       = ident_new("READ");
         ident_t write_i      = ident_new("WRITE");
         ident_t endfile_i    = ident_new("ENDFILE");

         type_t open_kind   = sem_std_type("FILE_OPEN_KIND");
         type_t open_status = sem_std_type("FILE_OPEN_STATUS");

         tree_t file_open1 = sem_builtin_proc(file_open_i, "file_open1");
         sem_add_port(file_open1, t, PORT_INOUT, NULL);
         sem_add_port(file_open1, std_string, PORT_IN, NULL);
         sem_add_port(file_open1, open_kind, PORT_IN, make_ref(read_mode));
         scope_insert(file_open1);
         tree_add_op(decl, file_open1);

         tree_t file_open2 = sem_builtin_proc(file_open_i, "file_open2");
         sem_add_port(file_open2, open_status, PORT_OUT, NULL);
         sem_add_port(file_open2, t, PORT_INOUT, NULL);
         sem_add_port(file_open2, std_string, PORT_IN, NULL);
         sem_add_port(file_open2, open_kind, PORT_IN, make_ref(read_mode));
         scope_insert(file_open2);
         tree_add_op(decl, file_open2);

         tree_t file_close = sem_builtin_proc(file_close_i, "file_close");
         sem_add_port(file_close, t, PORT_INOUT, NULL);
         scope_insert(file_close);
         tree_add_op(decl, file_close);

         tree_t read = sem_builtin_proc(read_i, "file_read");
         sem_add_port(read, t, PORT_INOUT, NULL);
         sem_add_port(read, of, PORT_OUT, NULL);
         if (type_is_array(of) && type_is_unconstrained(of))
            sem_add_port(read, std_int, PORT_OUT, NULL);
         scope_insert(read);
         tree_add_op(decl, read);

         tree_t write = sem_builtin_proc(write_i, "file_write");
         sem_add_port(write, t, PORT_INOUT, NULL);
         sem_add_port(write, of, PORT_IN, NULL);
         scope_insert(write);
         tree_add_op(decl, write);

         sem_declare_unary(decl, endfile_i, t, std_bool, "endfile");
      }
      break;

   case T_ACCESS:
      {
         ident_t deallocate_i = ident_new("DEALLOCATE");

         tree_t deallocate = sem_builtin_proc(deallocate_i, "deallocate");
         sem_add_port(deallocate, t, PORT_INOUT, NULL);
         scope_insert(deallocate);
         tree_add_op(decl, deallocate);
      }
      break;

   default:
      break;
   }

   // Predefined attributes

   switch (kind) {
   case T_SUBTYPE:
      if (type_is_unconstrained(t) || type_is_record(t))
         break;
      // Fall-through
   case T_INTEGER:
   case T_REAL:
   case T_PHYSICAL:
      {
         range_t r = type_dim(t, 0);

         tree_add_attr_tree(decl, ident_new("LEFT"), r.left);
         tree_add_attr_tree(decl, ident_new("RIGHT"), r.right);
         tree_add_attr_tree(decl, ident_new("ASCENDING"),
                            sem_bool_lit(std_bool, r.kind == RANGE_TO));

         if (r.kind == RANGE_TO) {
            tree_add_attr_tree(decl, ident_new("LOW"), r.left);
            tree_add_attr_tree(decl, ident_new("HIGH"), r.right);
         }
         else {
            tree_add_attr_tree(decl, ident_new("HIGH"), r.left);
            tree_add_attr_tree(decl, ident_new("LOW"), r.right);
         }

         ident_t image_i = ident_new("IMAGE");
         tree_add_attr_tree(decl, image_i,
                            sem_builtin_fn(image_i, std_string,
                                           "image", t, t, NULL));
      }
      break;

   case T_ENUM:
      {
         tree_t left  = type_enum_literal(t, 0);
         tree_t right = type_enum_literal(t, type_enum_literals(t) - 1);
         tree_add_attr_tree(decl, ident_new("LEFT"), make_ref(left));
         tree_add_attr_tree(decl, ident_new("RIGHT"), make_ref(right));
         tree_add_attr_tree(decl, ident_new("LOW"), make_ref(left));
         tree_add_attr_tree(decl, ident_new("HIGH"), make_ref(right));

         tree_t image = sem_builtin_fn(ident_new("IMAGE"),
                                       std_string, "image", t, t, NULL);
         tree_add_attr_tree(decl, ident_new("IMAGE"), image);

         ident_t pos_i = ident_new("POS");
         tree_add_attr_tree(decl, pos_i,
                            sem_builtin_fn(pos_i, std_int, "pos", t, t, NULL));

         ident_t val_i = ident_new("VAL");
         tree_add_attr_tree(decl, val_i,
                            sem_builtin_fn(val_i, t, "val",
                                           type_universal_int(), t, NULL));
      }
      break;

   default:
      break;
   }

   if (type_is_array(t)) {
      sem_add_dimension_attr(decl, sem_std_type("INTEGER"), "LENGTH", "length");
      sem_add_dimension_attr(decl, NULL, "LEFT", "left");
      sem_add_dimension_attr(decl, NULL, "RIGHT", "right");
      sem_add_dimension_attr(decl, NULL, "LOW", "low");
      sem_add_dimension_attr(decl, NULL, "HIGH", "high");
   }

   switch (type_kind(type_base_recur(t))) {
   case T_INTEGER:
   case T_PHYSICAL:
   case T_ENUM:
      {
         ident_t succ_i = ident_new("SUCC");
         tree_add_attr_tree(decl, succ_i,
                            sem_builtin_fn(succ_i, t, "succ", t, t, NULL));

         ident_t pred_i = ident_new("PRED");
         tree_add_attr_tree(decl, pred_i,
                            sem_builtin_fn(pred_i, t, "pred", t, t, NULL));

         ident_t leftof_i = ident_new("LEFTOF");
         tree_add_attr_tree(decl, leftof_i,
                            sem_builtin_fn(leftof_i, t, "leftof", t, t, NULL));

         ident_t rightof_i = ident_new("RIGHTOF");
         tree_add_attr_tree(decl, rightof_i,
                            sem_builtin_fn(rightof_i, t, "rightof",
                                           t, t, NULL));

         ident_t pos_i = ident_new("POS");
         tree_add_attr_tree(decl, pos_i,
                            sem_builtin_fn(pos_i, std_int, "pos", t, t, NULL));

         ident_t val_i = ident_new("VAL");
         tree_add_attr_tree(decl, val_i,
                            sem_builtin_fn(val_i, t, "val",
                                           type_universal_int(), t, NULL));
      }

      // Fall-through
   case T_REAL:
      {
         ident_t value_i = ident_new("VALUE");
         tree_add_attr_tree(decl, value_i,
                            sem_builtin_fn(value_i, t, "value",
                                           sem_std_type("STRING"), t, NULL));
      }
      break;

   default:
      break;
   }
}

static bool sem_check_resolution(type_t type)
{
   // Resolution functions are described in LRM 93 section 2.4

   assert(type_kind(type) == T_SUBTYPE);

   tree_t ref = type_resolution(type);

   tree_t fdecl = scope_find(tree_ident(ref));
   if (fdecl == NULL)
      sem_error(ref, "undefined resolution function %s",
                istr(tree_ident(ref)));

   tree_kind_t kind = tree_kind(fdecl);
   if ((kind != T_FUNC_DECL) && (kind != T_FUNC_BODY))
      sem_error(ref, "declaration %s is not a function",
                istr(tree_ident(ref)));

   type_t ftype = tree_type(fdecl);

   // Must take a single parameter of array of base type

   if (type_params(ftype) != 1)
      sem_error(fdecl, "resolution function must have single argument");

   type_t param = type_param(ftype, 0);
   if (type_kind(param) != T_UARRAY)
      sem_error(fdecl, "parameter of resolution function must be "
                "an unconstrained array type");

   if (!type_eq(type_elem(param), type))
      sem_error(fdecl, "parameter of resolution function must be "
                "array of %s", sem_type_str(type));

   // Return type must be the resolved type

   if (!type_eq(type_result(ftype), type))
      sem_error(fdecl, "result of resolution function must %s",
                sem_type_str(type));

   tree_set_ref(ref, fdecl);
   return true;
}

static bool sem_check_subtype(tree_t t, type_t type, type_t *pbase)
{
   // Resolve a subtype to its base type

   for (type_t base; type_kind(type) == T_SUBTYPE; type = base) {
      base = type_base(type);
      if (type_kind(base) == T_UNRESOLVED) {
         tree_t base_decl = scope_find(type_ident(base));
         if (base_decl == NULL)
            sem_error(t, "type %s is not declared", sem_type_str(base));

         base = tree_type(base_decl);
         type_set_base(type, base);
      }
      else
         continue;

      const type_kind_t base_kind = type_kind(base);

      // If the subtype is not constrained then give it the same
      // range as its base type
      const int ndims = type_dims(type);
      if (ndims == 0) {
         switch (base_kind) {
         case T_ENUM:
            {
               type_t std_int = sem_std_type("INTEGER");
               range_t r = {
                  .kind  = RANGE_TO,
                  .left  = sem_int_lit(std_int, 0),
                  .right = sem_int_lit(std_int, type_enum_literals(base) - 1)
               };
               type_add_dim(type, r);
            }
            break;

         case T_CARRAY:
         case T_SUBTYPE:
         case T_INTEGER:
         case T_REAL:
         case T_PHYSICAL:
            {
               const int ndims = type_dims(base);
               for (int i = 0; i < ndims; i++)
                  type_add_dim(type, type_dim(base, i));
            }
            break;

         case T_UARRAY:
         case T_RECORD:
            break;

         case T_PROTECTED:
            sem_error(t, "subtypes may not have protected base types");
            break;

         default:
            sem_error(t, "sorry, this form of subtype is not supported");
         }
      }
      else {
         // Check constraints

         if (type_is_record(base))
            sem_error(t, "record subtype may not have constraints");

         const int ndims_base =
            type_is_array(base)
            ? array_dimension(base)
            : ((base_kind == T_SUBTYPE) ? type_dims(base) : 1);

         if (ndims != ndims_base)
            sem_error(t, "expected %d constraints for type %s but found %d",
                      ndims_base, sem_type_str(base), ndims);

         for (int i = 0; i < ndims; i++) {
            range_t r = type_dim(type, i);
            if (!sem_check_range(&r, index_type_of(base, i)))
               return false;
            type_change_dim(type, i, r);
         }
      }

      if (type_has_resolution(type)) {
         if (!sem_check_resolution(type))
            return false;
      }
   }

   if (pbase)
      *pbase = type;

   return true;
}

static bool sem_declare(tree_t decl, bool add_predefined)
{
   // Handle special cases of scope insertion such as enumeration
   // literals, physical unit names, and predefined types

   // Certain kinds of declarations like components do not have
   // a type
   if (tree_kind(decl) == T_COMPONENT)
      return scope_insert(decl);

   // Resolve the base type if necessary
   if (!sem_check_subtype(decl, tree_type(decl), NULL))
      return false;

   // If this is a full type declarataion then replace any previous
   // incomplete type declaration
   tree_t forward = scope_find(tree_ident(decl));
   if (forward != NULL && tree_kind(forward) == T_TYPE_DECL) {
      type_t incomplete = tree_type(forward);
      if (type_kind(incomplete) == T_INCOMPLETE) {
         // Replace the incomplete type with the one we are defining
         type_replace(incomplete, tree_type(decl));
         tree_set_type(decl, incomplete);

         // Create a new incomplete type and attach that to the
         // forward declaration: this is useful when we serialise
         // the tree to avoid circular references
         type_t ni = type_new(T_INCOMPLETE);
         type_set_ident(ni, type_ident(incomplete));
         tree_set_type(forward, ni);

         scope_replace(forward, decl);
      }
   }
   else if (!scope_insert(decl))
      return false;

   type_kind_t type_k = type_kind(tree_type(decl));

   // Incomplete types cannot be checked any further
   if (type_k == T_INCOMPLETE)
      return true;

   if (tree_kind(decl) != T_TYPE_DECL)
      return true;

   // Declare any predefined operators and attributes
   if (add_predefined) {
      const int nops = tree_ops(decl);
      for (int i = 0; i < nops; i++)
         scope_insert(tree_op(decl, i));
   }

   // No futher processing needed for subtypes
   if (type_k == T_SUBTYPE)
      return true;

   bool ok = true;

   type_t type = tree_type(decl);
   switch (type_kind(type)) {
   case T_ENUM:
      // Need to add each literal to the scope
      {
         const int nlits = type_enum_literals(type);
         for (int i = 0; i < nlits; i++)
            ok = ok && scope_insert(type_enum_literal(type, i));
      }
      break;

   case T_PHYSICAL:
      // Add each unit to the scope
      {
         const int nunits = type_units(type);
         for (int i = 0; i < nunits; i++)
            ok = ok && scope_insert(type_unit(type, i));
      }
      break;

   default:
      break;
   }

   return ok;
}

static bool sem_check_range(range_t *r, type_t context)
{
   if (r->kind == RANGE_EXPR) {
      tree_t expr = r->left;
      assert(r->right == NULL);

      const bool is_attr_ref = (tree_kind(expr) == T_ATTR_REF);
      const bool reverse =
         is_attr_ref && (tree_ident(expr) == ident_new("REVERSE_RANGE"));

      assert(reverse || !is_attr_ref
             || (tree_ident(expr) == ident_new("RANGE")));
      assert(is_attr_ref || tree_kind(expr) == T_REF);

      ident_t name =
         is_attr_ref ? tree_ident(tree_name(expr)) : tree_ident(expr);

      tree_t decl = scope_find(name);
      if (decl == NULL)
         sem_error(expr, "no visible declaration for %s", istr(name));

      if (!class_has_type(class_of(decl)))
         sem_error(expr, "object %s does not have a range", istr(name));

      tree_t a = tree_new(T_ATTR_REF);
      tree_set_name(a, make_ref(decl));
      tree_set_ident(a, ident_new("LEFT"));
      tree_set_loc(a, tree_loc(expr));

      tree_t b = tree_new(T_ATTR_REF);
      tree_set_name(b, make_ref(decl));
      tree_set_ident(b, ident_new("RIGHT"));
      tree_set_loc(b, tree_loc(expr));

      if (type_kind(tree_type(decl)) == T_ACCESS) {
         sem_implicit_dereference(a, tree_name, tree_set_name);
         sem_implicit_dereference(b, tree_name, tree_set_name);
      }

      type_t type = tree_type(tree_name(a));
      const type_kind_t kind = type_kind(type);
      const bool is_unconstrained = type_is_unconstrained(type);

      bool is_static = false;

      if (tree_kind(decl) == T_TYPE_DECL) {
         if (kind == T_ENUM || kind == T_INTEGER || kind == T_SUBTYPE)
            is_static = false;
         else if (type_is_array(type) && !is_unconstrained)
            is_static = true;
         else
            sem_error(expr, "type %s does not have a range", istr(name));
      }
      else if (kind == T_SUBTYPE || type_is_array(type))
         is_static = !is_unconstrained;
      else
         sem_error(expr, "object %s does not have a range", istr(name));

      if (is_static) {
         range_t d0 = type_dim(type, 0);
         *r = type_dim(type, 0);
         if (reverse) {
            r->left  = b;
            r->right = a;
            r->kind  = (d0.kind == RANGE_TO) ? RANGE_DOWNTO : RANGE_TO;
         }
         else {
            r->left  = a;
            r->right = b;
            r->kind  = d0.kind;
         }
      }
      else {
         // If this is an unconstrained array then we can
         // only find out the direction at runtime
         r->kind  = (type_is_array(type)
                     ? (reverse ? RANGE_RDYN : RANGE_DYN)
                     : (reverse ? RANGE_DOWNTO : RANGE_TO));
         r->left  = a;
         r->right = b;
      }
   }

   if (!sem_check_constrained(r->left, context))
      return false;

   if (!sem_check_constrained(r->right, context))
      return false;

   type_t left_type  = tree_type(r->left);
   type_t right_type = tree_type(r->right);

   if (!type_eq(left_type, right_type))
      sem_error(r->right, "type mismatch in range: left is %s, right is %s",
                sem_type_str(left_type), sem_type_str(right_type));

   if (context == NULL) {
      if (type_is_universal(left_type) && type_is_universal(right_type)) {
         tree_kind_t lkind = tree_kind(r->left);
         tree_kind_t rkind = tree_kind(r->right);

         // See LRM 93 section 3.2.1.1
         // Later LRMs relax the wording here
         if (standard() < STD_00 && !(relax & RELAX_UNIVERSAL_BOUND)) {
            if ((lkind != T_LITERAL) && (lkind != T_ATTR_REF)
                && (rkind != T_LITERAL) && (rkind != T_ATTR_REF))
               sem_error(r->left, "universal integer bound must be "
                         "numeric literal or attribute");
         }

         // Implicit conversion to INTEGER
         type_t std_int = sem_std_type("INTEGER");
         tree_set_type(r->left, std_int);
         tree_set_type(r->right, std_int);
      }
   }

   return true;
}

static void sem_walk_lib(ident_t name, int kind, void *context)
{
   lib_walk_params_t *params = context;

   if (kind == T_PACKAGE) {
      if (scope_import_unit(name, params->lib, false, params->loc))
         params->error = true;
   }
}

static bool sem_check_use_clause(tree_t c)
{
   ident_t cname = tree_ident(c);

   const bool all = tree_has_ident2(c) && (tree_ident2(c) == all_i);

   ident_t lname = ident_until(cname, '.');

   lib_t lib = lib_find(istr(lname), false, false);
   if (lib != NULL) {
      if (lname == cname) {
         assert(all);

         lib_walk_params_t params = {
            .loc   = tree_loc(c),
            .lib   = lib,
            .error = false
         };
         lib_walk_index(lib, sem_walk_lib, &params);

         return params.error;
      }
      else if (scope_import_unit(cname, lib, all, tree_loc(c))) {
         if (tree_has_ident2(c) && !all) {
            ident_t full = ident_prefix(tree_ident(c), tree_ident2(c), '.');

            int n = 0;
            tree_t object;
            while ((object = scope_find_nth(full, n++)))
               scope_insert_alias(object, tree_ident2(c));

            if (n == 1)
               sem_error(c, "declaration %s not found in unit %s",
                         istr(tree_ident2(c)), istr(cname));
         }

         return true;
      }
      else
         return false;
   }
   else
      sem_error(c, "missing library clause for %s", istr(lname));
}

static bool sem_check_library_clause(tree_t t)
{
   if (lib_find(istr(tree_ident(t)), true, true) == NULL) {
      errors++;
      return false;
   }
   else
      return true;
}

static void sem_declare_universal(void)
{
   // Universal integers and reals have some additional overloaded operators
   // that are not valid for regular integer and real types
   // See LRM 93 section 7.5

   type_t uint  = type_universal_int();
   type_t ureal = type_universal_real();

   ident_t mult = ident_new("\"*\"");
   ident_t div  = ident_new("\"/\"");

   sem_declare_binary(NULL, mult, ureal, uint, ureal, "mulri");
   sem_declare_binary(NULL, mult, uint, ureal, ureal, "mulir");
   sem_declare_binary(NULL, div, ureal, uint, ureal, "divri");
}

static bool sem_check_context(tree_t t)
{
   // The std.standard package is also implicit unless we are
   // bootstrapping
   if (!opt_get_int("bootstrap")) {
      lib_t std = lib_find("std", true, true);
      if (std == NULL)
         fatal("failed to find std library");

      if (!scope_import_unit(std_standard_i, std, true, NULL))
         return false;

      sem_declare_universal();
   }

   bool ok = true;
   const int ncontexts = tree_contexts(t);
   for (int n = 0; n < ncontexts; n++)
      ok = sem_check(tree_context(t, n)) && ok;

   return ok;
}

static bool sem_check_constrained(tree_t t, type_t type)
{
   if ((type != NULL) && (type_kind(type) == T_UNRESOLVED))
      return false;

   type_set_push();
   if (type != NULL)
      type_set_add(type);
   bool ok = sem_check(t);
   type_set_pop();
   return ok;
}

static bool sem_readable(tree_t t)
{
   switch (tree_kind(t)) {
   case T_REF:
      {
         tree_t decl = tree_ref(t);
         if ((tree_kind(decl) == T_PORT_DECL)
             && (tree_subkind(decl) == PORT_OUT)
             && !(top_scope->flags & SCOPE_FORMAL))
            sem_error(t, "cannot read output port %s",
                      istr(tree_ident(t)));

         return true;
      }

   case T_ARRAY_REF:
   case T_ARRAY_SLICE:
      return sem_readable(tree_value(t));

   default:
      return true;
   }
}

static bool sem_check_array_dims(type_t type, type_t constraint)
{
   const int ndims = type_dims(type);
   for (int i = 0; i < ndims; i++) {
      range_t r = type_dim(type, i);

      type_t index_type = NULL;
      if (constraint != NULL)
         index_type = type_index_constr(constraint, i);
      else if (tree_has_type(r.left)) {
         // Index constraint of the form (FOO range A to B)
         // In this case the parser stores the type FOO with A and B
         index_type = tree_type(r.left);
         if (!sem_check_type(r.left, &index_type))
            return false;
      }

      if (!sem_check_range(&r, index_type))
         return false;

      if (index_type) {
         tree_t error = NULL;
         if (!type_eq(tree_type(r.left), index_type))
            error = r.left;
         else if (!type_eq(tree_type(r.right), index_type))
            error = r.right;

         if (error)
            sem_error(error, "type of bound %s does not match type of index %s",
                      sem_type_str(tree_type(error)),
                      sem_type_str(index_type));

         tree_set_type(r.left, index_type);
         tree_set_type(r.right, index_type);
      }

      type_change_dim(type, i, r);
   }

   return true;
}

static bool sem_check_type(tree_t t, type_t *ptype)
{
   // Check a type at the point where it is used not declared

   switch (type_kind(*ptype)) {
   case T_SUBTYPE:
      {
         type_t base;
         if (!sem_check_subtype(t, *ptype, &base))
            return false;

         if (type_kind(base) == T_UARRAY)
            return sem_check_array_dims(*ptype, base);
         else
            return true;
      }

   case T_UNRESOLVED:
      {
         tree_t type_decl = scope_find(type_ident(*ptype));
         if (type_decl == NULL)
            sem_error(t, "type %s is not declared", sem_type_str(*ptype));

         while (tree_kind(type_decl) == T_ALIAS) {
            tree_t value = tree_value(type_decl);
            if (tree_kind(value) != T_REF)
               break;

            type_decl = tree_ref(value);
         }

         if (tree_kind(type_decl) != T_TYPE_DECL)
            sem_error(t, "name %s does not refer to a type",
                      istr(tree_ident(type_decl)));

         *ptype = tree_type(type_decl);
      }
      return true;

   case T_FUNC:
      {
         type_t result = type_result(*ptype);
         if (!sem_check_type(t, &result))
            return false;
         type_set_result(*ptype, result);
      }

      // Fall-through
   case T_PROC:
      {
         const int nparams = type_params(*ptype);
         for (int i = 0; i < nparams; i++) {
            type_t param = type_param(*ptype, i);
            if (!sem_check_type(t, &param))
               return false;
            type_change_param(*ptype, i, param);
         }
      }
      return true;

   default:
      assert(false);
   }
}

static bool sem_check_type_decl(tree_t t)
{
   // We need to insert the type into the scope before performing
   // further checks as when bootstrapping we need INTEGER defined
   // before we can check any ranges. Adding a type with errors to
   // the symbol table should also avoid spurious type-not-defined
   // errors later on
   scope_apply_prefix(t);
   if (!sem_declare(t, false))
      return false;

   type_t type = tree_type(t);

   // Nothing more to do for incomplete types
   if (type_kind(type) == T_INCOMPLETE)
      return true;

   // Prefix the package name to the type name
   if (top_scope->prefix)
      type_set_ident(type, ident_prefix(top_scope->prefix,
                                        type_ident(type), '.'));

   type_t base;
   if (!sem_check_subtype(t, type, &base))
      return false;

   type_kind_t kind = type_kind(type);

   switch (kind) {
   case T_CARRAY:
   case T_UARRAY:
      {
         type_t elem_type = type_elem(base);
         if (!sem_check_type(t, &elem_type))
            return false;

         if (type_is_unconstrained(elem_type))
            sem_error(t, "array %s cannot have unconstrained element type",
                      istr(tree_ident(t)));

         type_set_elem(base, elem_type);
      }
      break;

   default:
      break;
   }

   switch (kind) {
   case T_CARRAY:
      {
         if (!sem_check_array_dims(base, NULL))
            return false;

         sem_declare_predefined_ops(t);
         return true;
      }

   case T_UARRAY:
      {
         const int nindex = type_index_constrs(type);
         for (int i = 0; i < nindex; i++) {
            type_t index_type = type_index_constr(type, i);
            if (!sem_check_type(t, &index_type))
               return false;

            type_change_index_constr(type, i, index_type);
         }

         sem_declare_predefined_ops(t);
         return true;
      }

   case T_PHYSICAL:
      {
         sem_declare_predefined_ops(t);

         // Check the units
         const int nunits = type_units(type);
         for (int i = 0; i < nunits; i++) {
            tree_t u = type_unit(type, i);
            tree_set_type(u, type);
            if (!sem_check_constrained(u, type))
               return false;
         }
      }

      // Fall-through
   case T_INTEGER:
   case T_REAL:
      {
         if (kind != T_PHYSICAL)
            sem_declare_predefined_ops(t);

         range_t r = type_dim(type, 0);

         if (!sem_check_constrained(r.left, type))
            return false;

         if (!sem_check_constrained(r.right, type))
            return false;

         // Standard specifies type of 'LEFT and 'RIGHT are same
         // as the declared type
         tree_set_type(r.left, type);
         tree_set_type(r.right, type);

         return true;
      }

   case T_SUBTYPE:
      {
         bool ok = true;
         const int ndims = type_dims(type);
         for (int i = 0; i < ndims; i++) {
            range_t r = type_dim(type, i);

            type_t index = NULL, alt = NULL;
            switch (type_kind(base)) {
            case T_CARRAY:
               index = tree_type(type_dim(base, i).left);
               break;
            case T_UARRAY:
               index = type_index_constr(base, i);
               break;
            case T_PHYSICAL:
               alt = sem_std_type("INTEGER");
               // Fall-through
            default:
               index = base;
               break;
            }

            if (type_kind(index) == T_UNRESOLVED)
               return false;

            type_set_push();
            type_set_add(index);
            if (alt != NULL)
               type_set_add(alt);

            ok = sem_check(r.left) && sem_check(r.right);
            type_set_pop();

            if (ok) {
               type_t rtype = type_is_scalar(base) ? type : index;
               tree_set_type(r.left, rtype);
               tree_set_type(r.right, rtype);
            }
         }

         if (!ok)
            return false;

         if (type_has_resolution(type)) {
            if (!sem_check_resolution(type))
               return false;
         }

         sem_declare_predefined_ops(t);
         return true;
      }

   case T_RECORD:
      {
         const int nfields = type_fields(type);
         for (int i = 0; i < nfields; i++) {
            tree_t f = type_field(type, i);

            if (!sem_check(f))
               return false;

            sem_add_attributes(f, true);

            // Each field name must be distinct
            ident_t f_name = tree_ident(f);
            for (int j = 0; j < i; j++) {
               if (f_name == tree_ident(type_field(type, j)))
                  sem_error(f, "duplicate field name %s", istr(f_name));
            }

            // Recursive record types are not allowed
            if (type_eq(type, tree_type(f)))
               sem_error(f, "recursive record types are not allowed");

            // Element types may not be unconstrained
            if (type_is_unconstrained(tree_type(f)))
               sem_error(f, "field %s with unconstrained array type "
                         "is not allowed", istr(f_name));
         }

         sem_declare_predefined_ops(t);
         return true;
      }

   case T_FILE:
      // Rules for file types are in LRM 93 section 3.4
      {
         type_t f = type_file(base);

         if (!sem_check_type(t, &f))
            return false;
         type_set_file(base, f);

         if (type_kind(f) == T_ACCESS)
            sem_error(t, "files may not be of access type");

         if (type_kind(f) == T_FILE)
            sem_error(t, "files may not be of file type");

         sem_declare_predefined_ops(t);
         return true;
      }

   case T_ACCESS:
      // Rules for access types are in LRM 93 section 3.3
      {
         type_t a = type_access(base);

         if (!sem_check_type(t, &a))
            return false;
         type_set_access(base, a);

         sem_declare_predefined_ops(t);
         return true;
      }

   case T_PROTECTED:
      // Rules for protected types are in LRM 02 section 3.5
      {
         scope_push(tree_ident(t));
         top_scope->flags |= SCOPE_PROTECTED;

         // Make protected type visible inside type definition
         scope_insert(t);
         ident_t unqual = ident_rfrom(tree_ident(t), '.');
         if (unqual != NULL)
            scope_insert_alias(t, unqual);

         bool ok = true;
         const int ndecls = type_decls(type);
         for (int i = 0; i < ndecls; i++) {
            tree_t d = type_decl(type, i);
            ok = sem_check(d) && ok;
         }

         scope_pop();
         return ok;
      }

   default:
      sem_declare_predefined_ops(t);
      return true;
   }
}

static tree_t sem_time_parameter_attribute(ident_t name, const char *builtin,
                                           type_t type, type_t result)
{
   // Helper for attributtes like 'STABLE and 'DELAYED that take an
   // optional TIME argument

   type_t std_time = sem_std_type("TIME");

   tree_t fn = sem_builtin_fn(name, result, builtin, std_time, type, NULL);
   tree_set_value(tree_port(fn, 0), sem_int_lit(std_time, 0));
   return fn;
}

static void sem_add_attributes(tree_t decl, bool is_signal)
{
   type_t std_bool = sem_std_type("BOOLEAN");

   type_t type;
   class_t class = class_of(decl);
   if (class_has_type(class))
      type = tree_type(decl);
   else
      type = type_new(T_NONE);

   // Implicit dereference for access types
   if (type_kind(type) == T_ACCESS)
      type = type_access(type);

   if (type_is_array(type)) {
      sem_add_dimension_attr(decl, sem_std_type("INTEGER"), "LENGTH", "length");
      sem_add_dimension_attr(decl, NULL, "LEFT", "left");
      sem_add_dimension_attr(decl, NULL, "RIGHT", "right");
      sem_add_dimension_attr(decl, NULL, "LOW", "low");
      sem_add_dimension_attr(decl, NULL, "HIGH", "high");
      sem_add_dimension_attr(decl, sem_std_type("BOOLEAN"),
                             "ASCENDING", "ascending");
   }

   if (is_signal) {
      type_t std_bit  = sem_std_type("BIT");

      ident_t event_i       = ident_new("EVENT");
      ident_t last_value_i  = ident_new("LAST_VALUE");
      ident_t active_i      = ident_new("ACTIVE");
      ident_t delayed_i     = ident_new("DELAYED");
      ident_t stable_i      = ident_new("STABLE");
      ident_t quiet_i       = ident_new("QUIET");
      ident_t transaction_i = ident_new("TRANSACTION");

      tree_add_attr_tree(decl, event_i,
                         sem_builtin_fn(event_i, std_bool, "event",
                                        type, NULL));
      tree_add_attr_tree(decl, active_i,
                         sem_builtin_fn(active_i, std_bool, "active",
                                        type, NULL));
      tree_add_attr_tree(decl, last_value_i,
                         sem_builtin_fn(last_value_i, type, "last_value",
                                        type, NULL));
      tree_add_attr_tree(decl, delayed_i,
                         sem_time_parameter_attribute(delayed_i, "delayed",
                                                      type, type));
      tree_add_attr_tree(decl, stable_i,
                         sem_time_parameter_attribute(delayed_i, "stable",
                                                      type, std_bool));
      tree_add_attr_tree(decl, quiet_i,
                         sem_time_parameter_attribute(delayed_i, "quiet",
                                                      type, std_bool));
      tree_add_attr_tree(decl, transaction_i,
                         sem_builtin_fn(transaction_i, std_bit,
                                        "transaction", type, NULL));
   }

   if (is_signal || (class == C_ARCHITECTURE) || (class == C_ENTITY)
       || (class == C_FUNCTION) || (class == C_PROCEDURE)
       || (class == C_LABEL)) {
      type_t std_string = sem_std_type("STRING");

      ident_t path_name_i  = ident_new("PATH_NAME");
      ident_t inst_name_i  = ident_new("INSTANCE_NAME");

      tree_add_attr_tree(decl, inst_name_i,
                         sem_builtin_fn(inst_name_i, std_string,
                                        "instance_name", type, NULL));
      tree_add_attr_tree(decl, path_name_i,
                         sem_builtin_fn(path_name_i, std_string,
                                        "path_name", type, NULL));
   }
}

static void sem_declare_fields(type_t type, ident_t prefix)
{
   // Insert a name into the scope for each field
   const int nfields = type_fields(type);
   for (int i = 0; i < nfields; i++) {
      tree_t field = type_field(type, i);
      ident_t qual = ident_prefix(prefix, tree_ident(field), '.');
      scope_insert_alias(field, qual);

      type_t field_type = tree_type(field);
      if (type_is_record(field_type))
         sem_declare_fields(field_type, qual);
   }
}

static void sem_declare_methods(type_t type, ident_t prefix)
{
   const int ndecls = type_decls(type);
   for (int i = 0; i < ndecls; i++) {
      tree_t decl = type_decl(type, i);
      if (tree_kind(decl) == T_USE)
         continue;

      ident_t suffix = ident_rfrom(tree_ident(decl), '.');
      ident_t qual = ident_prefix(prefix, suffix, '.');
      scope_insert_alias(decl, qual);

      type_t decl_type = tree_type(decl);
      if (type_is_record(decl_type))
         sem_declare_fields(decl_type, qual);
   }
}

static bool sem_check_decl(tree_t t)
{
   type_t type = tree_type(t);
   if (!sem_check_type(t, &type))
      return false;

   tree_set_type(t, type);

   tree_kind_t kind = tree_kind(t);

   if (!tree_has_value(t) && (kind == T_CONST_DECL)) {
      if (!(top_scope->flags & SCOPE_PACKAGE))
         sem_error(t, "deferred constant declarations are only permitted "
                   "in packages");
      else
         tree_add_attr_int(t, ident_new("deferred"), 1);
   }

   if (type_is_unconstrained(type) && (kind != T_CONST_DECL))
      sem_error(t, "type %s is unconstrained", sem_type_str(type));

   const bool needs_default_value =
      !tree_has_value(t) && (kind != T_PORT_DECL) && (kind != T_CONST_DECL)
      && (type_kind(type) != T_PROTECTED);

   if (needs_default_value)
      tree_set_value(t, make_default_value(type, tree_loc(t)));
   else if (tree_has_value(t)) {
      if (type_kind(type) == T_PROTECTED)
         sem_error(t, "variable %s with protected type may not have an "
                   "initial value", istr(tree_ident(t)));

      tree_t value = tree_value(t);
      if (!sem_check_constrained(value, type))
         return false;

      if (!type_eq(type, tree_type(value)))
         sem_error(value, "type of initial value %s does not match type "
                   "of declaration %s", sem_type_str(tree_type(value)),
                   sem_type_str(type));

      // Constant array declarations can be unconstrained and the size
      // is determined by the initialiser
      if ((kind == T_CONST_DECL) && type_is_unconstrained(type))
         tree_set_type(t, (type = tree_type(value)));
      else if (tree_kind(value) == T_LITERAL)
         tree_set_type(value, type);
   }

   if (kind == T_PORT_DECL && tree_class(t) == C_DEFAULT)
      tree_set_class(t, C_SIGNAL);

   const bool is_signal =
      ((kind == T_PORT_DECL) && (tree_class(t) == C_SIGNAL))
      || (kind == T_SIGNAL_DECL);

   if (type_is_record(type))
      sem_declare_fields(type, tree_ident(t));
   else if (type_is_protected(type))
      sem_declare_methods(type, tree_ident(t));
   else if (type_kind(type) == T_ACCESS) {
      type_t deref_type = type_access(type);
      if (type_is_record(deref_type)) {
         // Pointers to records can be dereferenced implicitly
         sem_declare_fields(deref_type, tree_ident(t));
      }

      if (kind == T_SIGNAL_DECL)
         sem_error(t, "signals may not have access type");
   }

   sem_add_attributes(t, is_signal);
   scope_apply_prefix(t);

   // From VHDL-2000 onwards shared variables must be protected types
   if ((standard() >= STD_00) && tree_attr_int(t, shared_i, 0)) {
      if (type_kind(type) != T_PROTECTED)
         sem_error(t, "shared variable %s must have protected type",
                   istr(tree_ident(t)));
   }

   // Check if we are giving a value to a deferred constant
   tree_t deferred;
   if ((kind == T_CONST_DECL)
       && (deferred = scope_find_in(tree_ident(t), top_scope, false, 0))) {

      if (tree_has_value(deferred))
         sem_error(t, "constant %s already has a value",
                   istr(tree_ident(deferred)));

      if (!type_eq(tree_type(deferred), type))
         sem_error(t, "expected type %s for deferred constant %s but found %s",
                   sem_type_str(tree_type(deferred)), istr(tree_ident(t)),
                   sem_type_str(type));

      tree_set_value(deferred, tree_value(t));
      return true;
   }
   else
      return scope_insert(t);
}

static bool sem_check_port_decl(tree_t t)
{
   type_t type = tree_type(t);
   if (!sem_check_type(t, &type))
      return false;

   tree_set_type(t, type);

   if (tree_has_value(t)) {
      tree_t value = tree_value(t);
      if (!sem_check_constrained(value, type))
         return false;

      if (!type_eq(type, tree_type(value)))
         sem_error(value, "type of default value %s does not match type "
                   "of declaration %s", sem_type_str(tree_type(value)),
                   sem_type_str(type));
   }

   if (type_is_record(type))
      sem_declare_fields(type, tree_ident(t));
   else if (type_kind(type) == T_ACCESS) {
      type_t deref_type = type_access(type);
      if (type_is_record(deref_type)) {
         // Pointers to records can be dereferenced implicitly
         sem_declare_fields(deref_type, tree_ident(t));
      }
   }

   sem_add_attributes(t, true);
   return true;
}

static bool sem_check_field_decl(tree_t t)
{
   type_t type = tree_type(t);
   if (!sem_check_type(t, &type))
      return false;

   tree_set_type(t, type);
   return true;
}

static bool sem_check_unit_decl(tree_t t)
{
   tree_t value = tree_value(t);
   if (!sem_check(value))
      return false;

   tree_set_type(value, tree_type(t));
   return true;
}

static bool sem_check_alias(tree_t t)
{
   // Rules for aliases are given in LRM 93 section 4.3.3

   tree_t value = tree_value(t);
   bool object = true;

   type_t type = NULL;
   if (tree_has_type(t))
       type = tree_type(t);

   if (type && (type_kind(type) == T_FUNC || type_kind(type) == T_PROC)) {
      // Alias of subprogram
      // Rules for matching signatures are in LRM 93 section 2.3.2

      if (tree_kind(value) != T_REF)
         sem_error(value, "invalid name in subprogram alias");

      if (!sem_check_type(t, &type))
         return false;
      tree_set_type(t, type);

      int n = 0;
      bool match = false;
      tree_t decl;
      do {
         if ((decl = scope_find_nth(tree_ident(value), n++))) {
            switch (tree_kind(decl)) {
            case T_FUNC_BODY:
            case T_FUNC_DECL:
            case T_PROC_BODY:
            case T_PROC_DECL:
               match = type_eq(type, tree_type(decl));
               break;
            default:
               break;
            }
         }
      } while (!match && decl != NULL);

      if (!match)
         sem_error(t, "no visible subprogram %s matches signature %s",
                   istr(tree_ident(value)), sem_type_str(type));

      tree_set_ref(value, decl);
      object = false;
   }
   else {
      // Check for alias of type
      tree_t decl = NULL;
      if (tree_kind(value) == T_REF && (decl = scope_find(tree_ident(value)))) {
         if (tree_kind(decl) == T_TYPE_DECL) {
            if (type != NULL)
               sem_error(t, "non-object alias may not have subtype indication");

            tree_set_ref(value, decl);
            tree_set_type(value, tree_type(decl));
            tree_set_type(t, tree_type(decl));
            object = false;
         }
      }
   }

   if (object) {
      if (!sem_check(value))
         return false;

      if (!sem_static_name(value))
         sem_error(value, "aliased name is not static");

      if (type != NULL) {
         if (!sem_check_type(t, &type))
            return false;

         if (!type_eq(type, tree_type(value)))
            sem_error(t, "type of aliased object %s does not match expected "
                      "type %s", sem_type_str(tree_type(value)),
                      sem_type_str(type));

         tree_set_type(t, type);
      }
      else
         tree_set_type(t, tree_type(value));
   }

   sem_add_attributes(t, false);

   scope_apply_prefix(t);
   return scope_insert(t);
}

static bool sem_check_access_class(tree_t port)
{
   // Access types must have variable class in LRM section 3.3

   type_kind_t kind = type_kind(tree_type(port));

   if ((tree_class(port) != C_VARIABLE) && (kind == T_ACCESS))
      sem_error(port, "object %s with access type must have class VARIABLE",
                istr(tree_ident(port)));

   return true;
}

static void sem_add_protected_arg(tree_t decl)
{
   tree_t prot = scope_find(top_scope->prefix);
   assert(prot);

   tree_t port = sem_add_port(decl, tree_type(prot), PORT_INOUT, NULL);
   tree_set_loc(port, tree_loc(decl));
   tree_set_ident(port, protected_i);
   tree_add_attr_int(port, protected_i, 1);
}

static bool sem_check_func_ports(tree_t t)
{
   type_t ftype = tree_type(t);

   const int nports = tree_ports(t);
   for (int i = 0; i < nports; i++) {
      tree_t p = tree_port(t, i);
      if (tree_subkind(p) != PORT_IN)
         sem_error(p, "function arguments must have mode IN");

      // See LRM 93 section 2.1.1 for default class
      if (tree_class(p) == C_VARIABLE)
         sem_error(p, "function arguments may not have VARIABLE class");

      if (!sem_check(p))
         return false;

      if (!sem_check_access_class(p))
         return false;

      type_add_param(ftype, tree_type(p));
   }

   type_t rtype = type_result(ftype);
   if (!sem_check_type(t, &rtype))
      return false;

   type_set_result(ftype, rtype);

   if (top_scope->flags & SCOPE_PROTECTED)
      sem_add_protected_arg(t);

   return true;
}

static bool sem_check_duplicate(tree_t t, tree_kind_t kind)
{
   tree_t decl;
   int n = 0;
   do {
      if ((decl = scope_find_nth(tree_ident(t), n++))) {
         if (tree_kind(decl) != kind)
            continue;

         if (type_eq(tree_type(t), tree_type(decl))) {
            // Allow builtin functions to be hidden
            if (tree_attr_str(decl, builtin_i) == NULL)
               break;
         }
      }
   } while (decl != NULL);

   return decl != NULL;
}

static bool sem_check_stmts(tree_t t, tree_t (*get_stmt)(tree_t, unsigned),
                            int nstmts)
{
   bool ok = true;
   for (int i = 0; i < nstmts; i++) {
      tree_t s = get_stmt(t, i);
      ok = scope_insert(s) && sem_check(s) && ok;
   }

   return ok;
}

static void sem_hoist_for_loop_var(tree_t t, void *context)
{
   // Move the declaration for the for loop induction variable to the
   // containing process or subprogram body

   const int ndecls = tree_decls(t);
   if (ndecls == 0)
      return;
   assert(ndecls == 1);

   tree_t container = context;
   tree_add_decl(container, tree_decl(t, 0));
}

static bool sem_check_func_decl(tree_t t)
{
   if (!sem_check_func_ports(t))
      return false;

   if (sem_check_duplicate(t, T_FUNC_DECL))
      sem_error(t, "duplicate declaration of function %s",
                istr(tree_ident(t)));

   sem_add_attributes(t, false);

   scope_apply_prefix(t);
   return scope_insert(t);
}

static bool sem_check_func_body(tree_t t)
{
   if (!sem_check_func_ports(t))
      return false;

   ident_t unqual = top_scope->prefix ? tree_ident(t) : NULL;
   scope_apply_prefix(t);

   sem_add_attributes(t, false);

   // If there is no declaration for this function add to the scope
   if (!sem_check_duplicate(t, T_FUNC_DECL)) {
      if (!scope_insert(t))
         return false;
   }

   scope_push(NULL);
   top_scope->subprog = t;

   if (unqual != NULL)
      scope_insert_alias(t, unqual);

   bool ok = true;

   const int nports = tree_ports(t);
   for (int i = 0; i < nports; i++) {
      tree_t p = tree_port(t, i);
      sem_add_attributes(p, (tree_class(p) == C_SIGNAL));
      ok = scope_insert(p) && ok;
   }

   const int ndecls = tree_decls(t);
   for (int i = 0; i < ndecls; i++)
      ok = sem_check(tree_decl(t, i)) && ok;

   ok = ok && sem_check_stmts(t, tree_stmt, tree_stmts(t));

   scope_pop();

   unsigned nret = tree_visit_only(t, NULL, NULL, T_RETURN);
   if (nret == 0)
      sem_error(t, "function must contain a return statement");

   tree_visit_only(t, sem_hoist_for_loop_var, t, T_FOR);

   return ok;
}

static bool sem_check_proc_ports(tree_t t)
{
   type_t ptype = tree_type(t);

   const int nports = tree_ports(t);
   for (unsigned i = 0; i < nports; i++) {
      tree_t p = tree_port(t, i);

      // See LRM 93 section 2.1.1 for default class
      if (tree_class(p) == C_DEFAULT) {
         switch (tree_subkind(p)) {
         case PORT_OUT:
         case PORT_INOUT:
            tree_set_class(p, C_VARIABLE);
            break;
         default:
            break;
         }
      }

      if (!sem_check(p))
         return false;

      if (!sem_check_access_class(p))
         return false;

      type_add_param(ptype, tree_type(p));
   }

   if (top_scope->flags & SCOPE_PROTECTED)
      sem_add_protected_arg(t);

   return true;
}

static bool sem_check_proc_decl(tree_t t)
{
   if (!sem_check_proc_ports(t))
      return false;

   if (sem_check_duplicate(t, T_PROC_DECL))
      sem_error(t, "duplicate declaration of procedure %s",
                istr(tree_ident(t)));

   sem_add_attributes(t, false);

   scope_apply_prefix(t);
   return scope_insert(t);
}

static bool sem_check_proc_body(tree_t t)
{
   if (!sem_check_proc_ports(t))
      return false;

   scope_apply_prefix(t);

   sem_add_attributes(t, false);

   // If there is no declaration for this procedure add to the scope
   if (!sem_check_duplicate(t, T_PROC_DECL)) {
      if (!scope_insert(t))
         return false;
   }

   scope_push(NULL);
   top_scope->subprog = t;

   bool ok = true;

   const int nports = tree_ports(t);
   for (int i = 0; i < nports; i++) {
      tree_t p = tree_port(t, i);
      sem_add_attributes(p, (tree_class(p) == C_SIGNAL));
      ok = scope_insert(p) && ok;
   }

   const int ndecls = tree_decls(t);
   for (int i = 0; i < ndecls; i++)
      ok = sem_check(tree_decl(t, i)) && ok;

   ok = ok && sem_check_stmts(t, tree_stmt, tree_stmts(t));

   tree_visit_only(t, sem_hoist_for_loop_var, t, T_FOR);

   scope_pop();
   return ok;
}

static bool sem_check_sensitivity(tree_t t)
{
   const int ntriggers = tree_triggers(t);
   for (int i = 0; i < ntriggers; i++) {
      tree_t r = tree_trigger(t, i);
      if (!sem_check(r) || !sem_readable(r))
         return false;

      // Can only reference signals in sensitivity list
      tree_t decl = sem_check_lvalue(r);
      if (decl == NULL)
         sem_error(r, "not a sutiable l-value");

      switch (tree_kind(decl)) {
      case T_SIGNAL_DECL:
      case T_PORT_DECL:
         break;
      default:
         sem_error(r, "name %s in sensitivity list is not a signal",
                   istr(tree_ident(decl)));
      }

      if (!sem_static_name(r))
         sem_error(r, "name in sensitivity list is not static");
   }

   return true;
}

static bool sem_check_process(tree_t t)
{
   sem_add_attributes(t, false);

   scope_push(NULL);

   bool ok = sem_check_sensitivity(t);

   const int ndecls = tree_decls(t);
   for (int n = 0; n < ndecls; n++)
      ok = sem_check(tree_decl(t, n)) && ok;

   ok = ok && sem_check_stmts(t, tree_stmt, tree_stmts(t));

   scope_pop();

   if (tree_triggers(t) > 0) {
      // No wait statements allowed in process with sensitivity list
      if (tree_visit_only(t, NULL, NULL, T_WAIT) > 0)
         sem_error(t, "wait statement not allowed in process "
                   "with sensitvity list");
   }

   tree_visit_only(t, sem_hoist_for_loop_var, t, T_FOR);

   return ok;
}

static bool sem_check_package(tree_t t)
{
   ident_t qual = ident_prefix(lib_name(lib_work()), tree_ident(t), '.');

   assert(top_scope == NULL);
   scope_push(NULL);

   const int ndecls = tree_decls(t);

   bool ok = sem_check_context(t);
   if (ok) {
      scope_push(qual);

      // Allow constant declarations without initial values
      top_scope->flags |= SCOPE_PACKAGE;

      for (int n = 0; n < ndecls; n++) {
         tree_t decl = tree_decl(t, n);
         ident_t unqual = tree_ident(decl);

         if (sem_check(decl)) {
            // Make the unqualified name visible inside the package
            if (tree_kind(decl) != T_ATTR_SPEC)
               scope_insert_alias(decl, unqual);
         }
         else
            ok = false;
      }
      scope_pop();
   }

   scope_pop();

   tree_set_ident(t, qual);
   lib_put(lib_work(), t);

   // Subprogram bodies are not allowed in package specification
   for (int i = 0; i < ndecls; i++) {
     tree_t d = tree_decl(t, i);
     tree_kind_t kind = tree_kind(d);
     if ((kind == T_FUNC_BODY) || (kind == T_PROC_BODY))
       sem_error(d, "subprogram body is not allowed in package specification");
   }

   return ok;
}

static bool sem_check_parameter_class_match(tree_t decl, tree_t body)
{
   const int nports = tree_ports(body);
   for (int k = 0; k < nports; k++) {
      tree_t pd = tree_port(decl, k);
      tree_t pb = tree_port(body, k);
      if (tree_class(pd) != tree_class(pb))
         sem_error(pb, "class %s of subprogram body %s paramteter %s does not "
                   "match class %s in declaration", class_str(tree_class(pb)),
                   istr(tree_ident(body)), istr(tree_ident(pb)),
                   class_str(tree_class(pd)));
   }

   return true;
}

static bool sem_check_missing_subprogram_body(tree_t body, tree_t spec)
{
   // Check for any subprogram declarations without bodies
   bool ok = true;
   const int ndecls = tree_decls(spec);
   for (int i = 0; i < ndecls; i++) {
      tree_t d = tree_decl(spec, i);
      tree_kind_t dkind = tree_kind(d);
      if (dkind == T_FUNC_DECL || dkind == T_PROC_DECL) {
         type_t dtype = tree_type(d);

         bool found = false;
         const int nbody_decls = tree_decls(body);
         const int start = (body == spec ? i + 1 : 0);
         for (int j = start; !found && (j < nbody_decls); j++) {
            tree_t b = tree_decl(body, j);
            tree_kind_t bkind = tree_kind(b);
            if (bkind == T_FUNC_BODY || bkind == T_PROC_BODY) {
               if (type_eq(dtype, tree_type(b))) {
                  found = true;
                  ok = sem_check_parameter_class_match(d, b) && ok;
               }
            }
         }

         if (!found && !opt_get_int("unit-test"))
            warn_at(tree_loc(d), "missing body for %s %s",
                    (dkind == T_FUNC_DECL) ? "function" : "procedure",
                    sem_type_str(dtype));
      }
   }

   if (body != spec)
      ok = sem_check_missing_subprogram_body(body, body) && ok;

   return ok;
}

static bool sem_check_package_body(tree_t t)
{
   ident_t qual = ident_prefix(lib_name(lib_work()), tree_ident(t), '.');

   assert(top_scope == NULL);
   scope_push(NULL);

   bool ok = sem_check_context(t);

   scope_push(qual);

   // Look up package declaration
   ok = ok && scope_import_unit(qual, lib_work(), true, tree_loc(t));

   tree_t pack = NULL;
   if (ok) {
      pack = lib_get_check_stale(lib_work(), qual);
      assert(pack != NULL);
      // XXX: this call should be in the outer scope above
      ok = ok && sem_check_context(pack);

      const int ndecls = tree_decls(t);
      for (int n = 0; n < ndecls; n++) {
         tree_t decl = tree_decl(t, n);
         ident_t unqual = tree_ident(decl);

         ok = sem_check(decl) && ok;

         // Make the unqualified name visible inside the package except
         // in the case of function and procedure bodies where the declaration
         // is already visible
         bool func_body_dup =
            (tree_kind(decl) == T_FUNC_BODY)
            && sem_check_duplicate(decl, T_FUNC_DECL);
         bool proc_body_dup =
            (tree_kind(decl) == T_PROC_BODY)
            && sem_check_duplicate(decl, T_PROC_DECL);
         bool make_visible = !func_body_dup && !proc_body_dup;

         if (make_visible)
            scope_insert_alias(decl, unqual);
      }
   }

   if (pack != NULL)
      ok = ok && sem_check_missing_subprogram_body(t, pack)
         && sem_check_missing_subprogram_body(t, t);

   scope_pop();
   scope_pop();

   if (pack != NULL) {
      // Check for any deferred constants which were not given values
      const int ndecls = tree_decls(pack);
      for (int i = 0; i < ndecls; i++) {
         tree_t d = tree_decl(pack, i);
         if ((tree_kind(d) == T_CONST_DECL) && !tree_has_value(d))
            sem_error(d, "deferred constant %s was not given a value in the "
                      "package body", istr(tree_ident(d)));
      }
   }

   if (ok) {
      tree_set_ident(t, ident_prefix(qual, ident_new("body"), '-'));
      lib_put(lib_work(), t);
   }

   return ok;
}

static bool sem_check_generics(tree_t t)
{
   bool ok = true;

   const int ngenerics = tree_generics(t);
   for (int n = 0; n < ngenerics; n++) {
      tree_t g = tree_generic(t, n);

      switch (tree_class(g)) {
      case C_DEFAULT:
         tree_set_class(g, C_CONSTANT);
         break;
      case C_CONSTANT:
         break;
      default:
         sem_error(g, "invalid object class for generic");
      }

      tree_add_attr_int(g, elab_copy_i, 1);

      ok = sem_check(g) && ok;
   }

   if (ok) {
      // Make generics visible in this region
      for (int n = 0; n < ngenerics; n++)
         ok = scope_insert(tree_generic(t, n)) && ok;
   }

   return ok;
}

static bool sem_check_ports(tree_t t)
{
   bool ok = true;

   const int nports = tree_ports(t);
   for (int n = 0; n < nports; n++) {
      tree_t p = tree_port(t, n);

      if (tree_class(p) == C_DEFAULT)
         tree_set_class(p, C_SIGNAL);

      tree_add_attr_int(p, elab_copy_i, 1);

      ok = sem_check(p) && ok;
   }

   return ok;
}

static bool sem_check_component(tree_t t)
{
   scope_push(NULL);

   bool ok = sem_check_generics(t) && sem_check_ports(t);

   scope_pop();

   sem_add_attributes(t, false);

   if (ok) {
      scope_apply_prefix(t);
      return scope_insert(t);
   }
   else
      return false;
}

static bool sem_check_entity(tree_t t)
{
   assert(top_scope == NULL);
   scope_push(NULL);

   bool ok = sem_check_context(t);

   scope_push(NULL);

   ok = ok && sem_check_generics(t) && sem_check_ports(t);

   scope_insert(t);

   sem_add_attributes(t, false);

   if (ok) {
      const int ndecls = tree_decls(t);
      const int nstmts = tree_stmts(t);

      if ((ndecls > 0) || (nstmts > 0)) {
         // Make ports visible in this region
         const int nports = tree_ports(t);
         for (int i = 0; i < nports; i++)
            scope_insert(tree_port(t, i));
      }

      for (int n = 0; n < ndecls; n++)
         ok = sem_check(tree_decl(t, n)) && ok;

      ok = ok && sem_check_stmts(t, tree_stmt, nstmts);
   }

   scope_pop();

   scope_pop();

   // Prefix the entity with the current library name
   ident_t qual = ident_prefix(lib_name(lib_work()), tree_ident(t), '.');
   tree_set_ident(t, qual);
   lib_put(lib_work(), t);

   return ok;
}

static bool sem_check_arch(tree_t t)
{
   // Find the corresponding entity
   tree_t e = lib_get_check_stale(lib_work(),
                                  ident_prefix(lib_name(lib_work()),
                                               tree_ident2(t), '.'));
   if (e == NULL)
      sem_error(t, "missing declaration for entity %s",
                istr(tree_ident2(t)));

   if (tree_kind(e) != T_ENTITY)
      sem_error(t, "unit %s is not an entity", istr(tree_ident(e)));

   tree_set_ref(t, e);

   assert(top_scope == NULL);
   scope_push(NULL);

   // Make all port and generic declarations available in this scope

   bool ok = sem_check_context(e) && sem_check_context(t);

   scope_push(NULL);

   sem_add_attributes(t, false);

   // Make the architecture and entity name visible
   scope_insert(t);
   scope_insert(e);
   scope_insert_alias(e, tree_ident2(t));

   scope_push(NULL);

   const int nports = tree_ports(e);
   for (int n = 0; n < nports; n++) {
      tree_t p = tree_port(e, n);
      scope_insert(p);

      type_t type = tree_type(p);
      if (type_is_record(type))
         sem_declare_fields(type, tree_ident(p));
   }

   const int ngenerics = tree_generics(e);
   for (int n = 0; n < ngenerics; n++)
      scope_insert(tree_generic(e, n));

   const int ndecls_ent = tree_decls(e);
   for (int n = 0; n < ndecls_ent; n++) {
      tree_t d = tree_decl(e, n);
      if (tree_kind(d) != T_ATTR_SPEC)
         sem_declare(d, true);
   }

   ok = ok && sem_check_missing_subprogram_body(t, t);

   // Now check the architecture itself

   const int ndecls = tree_decls(t);
   for (int n = 0; n < ndecls; n++)
      ok = sem_check(tree_decl(t, n)) && ok;

   ok = ok && sem_check_stmts(t, tree_stmt, tree_stmts(t));

   ok = scope_run_deferred_checks() && ok;

   scope_pop();
   scope_pop();
   scope_pop();

   // Prefix the architecture with the current library and entity name
   ident_t lname = lib_name(lib_work());
   ident_t qual = ident_prefix(ident_prefix(lname, tree_ident2(t), '.'),
                               tree_ident(t), '-');
   tree_set_ident(t, qual);
   ident_t ent_qual = ident_prefix(lname, tree_ident2(t), '.');
   tree_set_ident2(t, ent_qual);

   lib_put(lib_work(), t);

   return ok;
}

static tree_t sem_check_lvalue(tree_t t)
{
   switch (tree_kind(t)) {
   case T_REF:
      return sem_check_lvalue(tree_ref(t));
   case T_ARRAY_SLICE:
   case T_ARRAY_REF:
   case T_ALIAS:
   case T_RECORD_REF:
   case T_ALL:
      return sem_check_lvalue(tree_value(t));
   case T_VAR_DECL:
   case T_SIGNAL_DECL:
   case T_PORT_DECL:
   case T_CONST_DECL:
      return t;
   default:
      return NULL;
   }
}

static bool sem_check_var_assign(tree_t t)
{
   tree_t target = tree_target(t);
   tree_t value = tree_value(t);

   bool ok = sem_check(target);
   if (!ok)
      return false;

   ok = sem_check_constrained(value, tree_type(target));
   if (!ok)
      return false;

   ok = sem_readable(value);
   if (!ok)
      return false;

   tree_t decl = sem_check_lvalue(target);
   if (decl == NULL)
      sem_error(target, "not a suitable l-value");

   bool suitable = (tree_kind(decl) == T_VAR_DECL)
      || (tree_kind(decl) == T_PORT_DECL && tree_class(decl) == C_VARIABLE);

   if (!suitable)
      sem_error(target, "invalid target of variable assignment");

   type_t target_type = tree_type(target);
   type_t value_type  = tree_type(value);

   if (!type_eq(target_type, value_type))
      sem_error(t, "type of value %s does not match type of target %s",
                sem_type_str(value_type),
                sem_type_str(target_type));

   if (type_is_universal(value_type))
      tree_set_type(value, target_type);

   return ok;
}

static bool sem_check_waveforms(tree_t t, type_t expect)
{
   type_t std_time = sem_std_type("TIME");

   for (unsigned i = 0; i < tree_waveforms(t); i++) {
      tree_t waveform = tree_waveform(t, i);
      tree_t value = tree_value(waveform);

      if (!sem_check_constrained(value, expect))
         return false;

      if (!sem_readable(value))
         return false;

      type_t value_type = tree_type(value);

      if (!type_eq(expect, value_type))
         sem_error(t, "type of value %s does not match type of target %s",
                   sem_type_str(value_type), sem_type_str(expect));

      if (type_is_universal(value_type))
         tree_set_type(value, expect);

      if (tree_has_delay(waveform)) {
         tree_t delay = tree_delay(waveform);
         if (!sem_check(delay))
            return false;

         if (!type_eq(tree_type(delay), std_time))
            sem_error(delay, "type of delay must be %s",
                      sem_type_str(std_time));
      }
   }

   return true;
}

static bool sem_check_signal_target(tree_t target)
{
   if (tree_kind(target) == T_AGGREGATE) {
      // Rules for aggregate signal targets in LRM 93 section 8.4
      const int nassocs = tree_assocs(target);
      for (int i = 0; i < nassocs; i++) {
         tree_t a = tree_assoc(target, i);
         tree_t value = tree_value(a);

         if (!sem_check_signal_target(value))
            return false;

         if (!sem_static_name(value))
            sem_error(value, "aggregate element must be locally static name");

         assoc_kind_t kind = tree_subkind(a);
         switch (kind) {
         case A_OTHERS:
            sem_error(a, "others association not allowed in aggregate "
                      "signal target");
         case A_RANGE:
            sem_error(a, "range association not allowed in aggregate "
                      "signal target");
         case A_NAMED:
            sem_error(a, "sorry, named associations are not yet "
                      "supported here");
         case A_POS:
            break;
         }
      }

      return true;
   }
   else {
      tree_t decl = sem_check_lvalue(target);
      if (decl == NULL)
         sem_error(target, "not a suitable l-value");

      switch (tree_kind(decl)) {
      case T_SIGNAL_DECL:
         break;

      case T_PORT_DECL:
         if (tree_subkind(decl) == PORT_IN)
            sem_error(target, "cannot assign to input port %s",
                      istr(tree_ident(decl)));
         else if (tree_class(decl) != C_SIGNAL)
            sem_error(target, "target of signal assignment is not a signal");
         break;

      default:
         sem_error(target, "invalid target of signal assignment");
      }

      return true;
   }
}

static bool sem_check_reject(tree_t t)
{
   if (!sem_check(t))
      return false;

   type_t std_time = sem_std_type("TIME");
   if (!type_eq(tree_type(t), std_time))
      sem_error(t, "reject interval must have type TIME");

   return true;
}

static bool sem_check_signal_assign(tree_t t)
{
   tree_t target = tree_target(t);

   type_t context = NULL;
   if (tree_kind(target) == T_AGGREGATE) {
      // Context for target is type of RHS
      tree_t w0 = tree_value(tree_waveform(t, 0));
      if (!sem_check(w0))
         return false;
      context = tree_type(w0);
   }

   if (!sem_check_constrained(target, context))
      return false;

   if (!sem_check_waveforms(t, tree_type(target)))
      return false;

   if (!sem_check_signal_target(target))
      return false;

   if (!sem_check_reject(tree_reject(t)))
      return false;

   return true;
}

static bool sem_check_cassign(tree_t t)
{
   tree_t target = tree_target(t);

   type_t context = NULL;
   if (tree_kind(target) == T_AGGREGATE) {
      // Context for target is type of RHS
      tree_t w0 = tree_value(tree_waveform(tree_cond(t, 0), 0));
      if (!sem_check(w0))
         return false;
      context = tree_type(w0);
   }

   if (!sem_check_constrained(target, context))
      return false;

   type_t std_bool = sem_std_type("BOOLEAN");

   const int nconds = tree_conds(t);
   for (int i = 0; i < nconds; i++) {
      tree_t c = tree_cond(t, i);

      if (tree_has_value(c)) {
         tree_t test = tree_value(c);

         if (!sem_check_constrained(test, std_bool))
            return false;

         if (!type_eq(tree_type(test), std_bool))
            sem_error(test, "type of condition must be BOOLEAN");
      }

      if (!sem_check_reject(tree_reject(c)))
         return false;

      if (!sem_check_waveforms(c, tree_type(target)))
         return false;
   }

   if (!sem_check_signal_target(target))
      return false;

   return true;
}

static bool sem_check_conversion(tree_t t)
{
   // Type conversions are described in LRM 93 section 7.3.5

   if (tree_params(t) != 1)
      sem_error(t, "type conversions must have exactly one parameter");

   // Really we should push the set of types that are closely related
   // to the one being converted to
   tree_t p = tree_param(t, 0);
   if (!sem_check_constrained(tree_value(p), NULL))
      return false;

   type_t from = tree_type(tree_value(p));
   type_t to   = tree_type(tree_ref(t));

   tree_set_type(t, to);

   // Resolve both types to their base types
   from = type_base_recur(from);
   to   = type_base_recur(to);

   type_kind_t from_k = type_kind(from);
   type_kind_t to_k   = type_kind(to);

   const bool from_num = (from_k == T_INTEGER) || (from_k == T_REAL);
   const bool to_num   = (to_k == T_INTEGER) || (to_k == T_REAL);

   // Conversions are allowed between any abstract numeric types
   if (from_num && to_num)
      return true;

   const bool from_array = (from_k == T_CARRAY || from_k == T_UARRAY);
   const bool to_array   = (to_k == T_CARRAY || to_k == T_UARRAY);

   if (from_array && to_array) {
      // Types must have same dimensionality
      bool same_dim = (array_dimension(from) == array_dimension(to));

      // TODO: index types the same or closely related

      // Element types must be the same
      bool same_elem = type_eq(type_elem(from), type_elem(to));

      if (same_dim && same_elem)
         return true;
   }

   sem_error(t, "conversion only allowed between closely related types");
}

static int sem_ambiguous_rate(tree_t t)
{
   switch (tree_kind(t)) {
   case T_REF:
      {
         tree_t decl = scope_find(tree_ident(t));
         return (decl != NULL && tree_kind(decl) == T_ENUM_LIT) ? 50 : 0;
      }
   case T_AGGREGATE:
      return 100;
   case T_CONCAT:
      return 40;
   case T_LITERAL:
      {
         switch (tree_subkind(t)) {
         case L_NULL:   return 0;
         case L_STRING: return 90;
         default:       return -10;
         }
      }
   case T_TYPE_CONV:
      return -50;
   case T_FCALL:
      {
         tree_t decl = scope_find(tree_ident(t));
         return (decl != NULL) && (tree_kind(decl) == T_TYPE_DECL) ? -40 : 0;
      }
   default:
      return 0;
   }
}

static int sem_ambiguous_cmp(const void *_a, const void *_b)
{
   tree_t a = *(tree_t *)_a;
   tree_t b = *(tree_t *)_b;

   return sem_ambiguous_rate(tree_value(a)) - sem_ambiguous_rate(tree_value(b));
}

static void sem_sort_ambiguous(tree_t *params, int n)
{
   // Reorder a list of tree so the unambiguous ones such as literals
   // are checked first

   qsort(params, n, sizeof(tree_t), sem_ambiguous_cmp);
}

static type_t sem_find_param_type(tree_t param, tree_t decl)
{
   type_t decl_type = tree_type(decl);

   switch (tree_subkind(param)) {
   case P_POS:
      // Simple case of positional parameters is just the same index
      // into the port list
      return type_param(decl_type, tree_pos(param));

   case P_NAMED:
      // Need to search through the port list for a matching name
      {
         tree_t ref = tree_name(param);
         assert(tree_kind(ref) == T_REF);

         const int nports = tree_ports(decl);
         ident_t param_name = tree_ident(ref);
         for (int i = 0; i < nports; i++) {
            if (tree_ident(tree_port(decl, i)) == param_name)
               return type_param(decl_type, i);
         }

         return NULL;
      }

   default:
      assert(false);
   }
}

static bool sem_resolve_overload(tree_t t, tree_t *pick, int *matches,
                                 tree_t *overloads, int n_overloads)
{
   *pick    = NULL;
   *matches = 0;

   const int nparams = tree_params(t);

   // Work out which parameters have ambiguous interpretations
   tree_t order[nparams];
   for (int i = 0; i < nparams; i++)
      order[i] = tree_param(t, i);

   sem_sort_ambiguous(order, nparams);

   for (int i = 0; i < nparams; i++) {
      type_set_push();

      tree_t p = order[i];
      type_t param_types[n_overloads];

      for (int j = 0; j < n_overloads; j++) {
         if (overloads[j] != NULL) {
            param_types[j] = sem_find_param_type(p, overloads[j]);
            if (param_types[j] != NULL)
               type_set_add(param_types[j]);
         }
      }

      bool ok = sem_check(tree_value(p));

      type_set_pop();

      if (ok) {
         // Delete all overloads which don't match this parameter type
         type_t ptype = tree_type(tree_value(p));
         for (int j = 0; j < n_overloads; j++) {
            if ((overloads[j] != NULL) && (param_types[j] != NULL)) {
               if (!type_eq(param_types[j], ptype))
                  overloads[j] = NULL;

               if (type_is_universal(param_types[j])) {
                  // Universal operators only match universal arguments
                  if (!type_is_universal(ptype))
                     overloads[j] = NULL;
               }
            }
         }
      }
      else
         return false;
   }

   for (int n = 0; n < n_overloads; n++) {
      if (overloads[n] == NULL)
         continue;

      // Did argument types match for this overload?
      bool match = true;
      bool all_universal = true;
      for (int i = 0; i < nparams; i++) {
         tree_t p = tree_param(t, i);
         type_t ptype = tree_type(tree_value(p));
         type_t mtype = sem_find_param_type(p, overloads[n]);
         if (mtype == NULL)
            match = false;
         else {
            match = match && type_eq(mtype, ptype);
            all_universal = all_universal && type_is_universal(ptype);
         }
      }

      if (match) {
         (*matches)++;
         bool builtin = tree_attr_str(overloads[n], builtin_i);
         if (all_universal && builtin) {
            // If all the arguments are universal integer or real and
            // this is a builtin function then it doesn't matter which
            // overload we pick as it will be constant-folded later
            type_t f_result = type_result(tree_type(overloads[n]));
            switch (type_kind(f_result)) {
            case T_INTEGER:
               tree_set_type(t, type_universal_int());
               break;
            case T_REAL:
               tree_set_type(t, type_universal_real());
               break;
            default:
               tree_set_type(t, f_result);
            }
            tree_set_ref(t, overloads[n]);
            return true;
         }
         else
            *pick = overloads[n];
      }
      else
         overloads[n] = NULL;
   }

   return true;
}

static int sem_required_args(tree_t decl)
{
   // Count the number of non-default arguments
   const int ndecls = tree_ports(decl);
   int n = 0;
   for (int i = 0; i < ndecls; i++) {
      tree_t port = tree_port(decl, i);
      if (!tree_has_value(port) && !tree_attr_int(port, protected_i, 0))
         n++;
   }

   return n;
}

static bool sem_check_arity(tree_t call, tree_t decl)
{
   const int nparams = tree_params(call);
   const int nports  = tree_ports(decl);
   const int nreq    = sem_required_args(decl);

   if (nports < nparams)
      return false;
   else
      return (nparams == nports) || (nparams >= nreq);
}

static bool sem_copy_default_args(tree_t call, tree_t decl)
{
   const int nparams = tree_params(call);
   const int nports  = tree_ports(decl);

   // Copy the default values for any unspecified arguments
   for (int i = 0; i < nports; i++) {
      tree_t port  = tree_port(decl, i);
      ident_t name = tree_ident(port);

      tree_t found = NULL;
      for (int j = 0; (j < nparams) && (found == NULL); j++) {
         tree_t p = tree_param(call, j);
         switch (tree_subkind(p)) {
         case P_POS:
            if (tree_pos(p) == i)
               found = p;
            break;
         case P_NAMED:
            {
               tree_t ref = tree_name(p);
               if (tree_ident(ref) == name) {
                  found = p;
                  tree_set_ref(ref, port);
               }
            }
            break;
         default:
            assert(false);
         }
      }

      if (found == NULL) {
         if (tree_has_value(port))
            found = add_param(call, tree_value(port), P_NAMED, make_ref(port));
         else if (tree_attr_int(port, protected_i, 0)) {
            ident_t name = (tree_kind(call) == T_PCALL)
               ? tree_ident2(call) : tree_ident(call);
            ident_t prefix = ident_until(name, '.');

            tree_t var = NULL;
            if (prefix == name) {
               // Call to protected subprogram from inside protected object
               assert(top_scope->flags & SCOPE_PROTECTED);
               var = scope_find(protected_i);
               assert(var && tree_kind(var) == T_PORT_DECL);
            }
            else {
               var = scope_find(prefix);
               assert(var && tree_kind(var) == T_VAR_DECL);
            }

            add_param(call, make_ref(var), P_NAMED, make_ref(port));
            continue;
         }
         else
            sem_error(call, "missing actual for formal %s without "
                      "default value", istr(name));
      }

      // Constrain the type of any universal arguments
      tree_t value = tree_value(found);
      if (tree_has_type(value) && type_is_universal(tree_type(value)))
         tree_set_type(value, tree_type(port));

      // Check IN and INOUT parameters can be read
      if (tree_kind(call) != T_ATTR_REF) {
         port_mode_t mode = tree_subkind(port);
         if ((mode == PORT_IN) || (mode == PORT_INOUT)) {
            if (!sem_readable(value))
               return false;
         }
      }
   }

   return true;
}

static bool sem_check_params(tree_t t)
{
   bool have_named = false;
   const int nparams = tree_params(t);
   for (int i = 0; i < nparams; i++) {
      tree_t p = tree_param(t, i);

      switch (tree_subkind(p)) {
      case P_POS:
         if (have_named)
            sem_error(p, "positional parameters must precede named "
                      "parameters");
         break;

      case P_NAMED:
         {
            tree_t ref = tree_name(p);
            if (tree_kind(ref) != T_REF)
               sem_error(ref, "sorry, this form of parameter name "
                        "is not yet supported");

            ident_t name = tree_ident(ref);
            for (int j = 0; j < i; j++) {
               tree_t q = tree_param(t, j);
               if ((tree_subkind(q) == P_NAMED)
                   && (tree_ident(tree_name(q)) == name))
                  sem_error(p, "duplicate parameter name %s", istr(name));
            }

            have_named = true;
         }
         break;
      }
   }

   return true;
}

static bool sem_check_fcall(tree_t t)
{
   if (!sem_check_params(t))
      return false;

   int n_overloads = 0;
   int max_overloads = 128;
   tree_t *overloads LOCAL = xmalloc(max_overloads * sizeof(tree_t));

   const bool prefer_explicit = relax & RELAX_PREFER_EXPLICT;

   tree_t decl;
   ident_t name = tree_ident(t);
   int n = 0, found_func = 0;
   do {
      if ((decl = scope_find_nth(name, n++))) {
         type_t func_type = tree_type(decl);

         switch (tree_kind(decl)) {
         case T_FUNC_DECL:
         case T_FUNC_BODY:
            found_func++;
            break;
         case T_TYPE_DECL:
            tree_change_kind(t, T_TYPE_CONV);
            tree_set_ref(t, decl);
            return sem_check_conversion(t);
         case T_ALIAS:
            if (tree_has_type(decl) && type_kind(func_type) == T_FUNC) {
               decl = tree_ref(tree_value(decl));
               func_type = tree_type(decl);
               found_func++;
               break;
            }
            // Fall-through
         default:
            if (type_is_array(func_type)
                || ((type_kind(func_type) == T_ACCESS)
                    && type_is_array(type_access(func_type)))) {
               // The grammar is ambiguous between function calls and
               // array references so must be an array reference
               tree_t ref = tree_new(T_REF);
               tree_set_ident(ref, name);
               tree_set_loc(ref, tree_loc(t));

               tree_change_kind(t, T_ARRAY_REF);
               tree_set_value(t, ref);

               return sem_check_array_ref(t);
            }
            else
               continue;   // Look for the next matching name
         }

         if (type_set_member(type_result(func_type))) {
            // Number of arguments must match
            if (!sem_check_arity(t, decl))
               continue;

            // Same function may appear multiple times in the symbol
            // table under different names
            bool duplicate = false;
            for (int i = 0; i < n_overloads; i++) {
               if (overloads[i] == decl)
                  duplicate = true;
               else if (type_eq(tree_type(overloads[i]), func_type)) {
                  const bool same_name =
                     (tree_ident(overloads[i]) == tree_ident(decl));

                  const bool hide_implicit =
                     ((tree_attr_str(decl, builtin_i) != NULL)
                      || (tree_attr_str(overloads[i], builtin_i) != NULL))
                     && prefer_explicit;

                  if (same_name || hide_implicit)
                     duplicate = true;
               }
            }

            if (!duplicate) {
               // Found a matching function definition
               ARRAY_APPEND(overloads, decl, n_overloads, max_overloads);
            }
         }
      }
   } while (decl != NULL);

   if (n_overloads == 0)
      sem_error(t, (found_func > 0
                    ? "no matching function %s"
                    : "no visible declaration for %s"),
                istr(name));

   int matches;
   if (!sem_resolve_overload(t, &decl, &matches, overloads, n_overloads))
      return false;

   if (matches > 0 && decl == NULL)
      return true;   // Resolved to a builtin function

   if (matches > 1) {
      LOCAL_TEXT_BUF tb = tb_new();

      const bool operator = !isalpha((int)*istr(name));

      int nimplicit = 0, nexplicit = 0;
      for (int n = 0; n < n_overloads; n++) {
         if (overloads[n] != NULL) {
            const bool implicit = tree_attr_str(overloads[n], builtin_i);
            tb_printf(tb, "\n    %s%s",
                      sem_type_str(tree_type(overloads[n])),
                      implicit ? " (implicit)" : "");
            if (implicit)
               nimplicit++;
            else
               nexplicit++;
         }
      }

      if ((nimplicit == 1) && (nexplicit == 1))
         tb_printf(tb, "\nYou can use the --relax=prefer-explicit option to "
                   "hide the implicit %s",
                   operator ? "operator" : "function");

      sem_error(t, "ambiguous %s %s%s",
                operator ? "use of operator" : "call to function",
                istr(name), tb_get(tb));
   }

   if (decl == NULL) {
      LOCAL_TEXT_BUF tb = tb_new();

      const char *fname = istr(name);
      const bool operator = !isalpha((int)fname[0]);
      const char *quote = (operator && fname[0] != '"') ? "\"" : "";

      tb_printf(tb, "%s%s%s [", quote, fname, quote);
      for (unsigned i = 0; i < tree_params(t); i++)
         tb_printf(tb, "%s%s",
                   (i == 0 ? "" : ", "),
                   sem_type_str(tree_type(tree_value(tree_param(t, i)))));

      if ((top_type_set != NULL) && (top_type_set->n_members > 0)) {
         tb_printf(tb, " return");
         for (int i = 0; i < top_type_set->n_members; i++)
            tb_printf(tb, "%s %s",
                      (i > 0 ? " |" : ""),
                      sem_type_str(top_type_set->members[i]));
      }

      tb_printf(tb, "]");

      sem_error(t, (n == 1 ? "undefined %s %s"
                    : "no suitable overload for %s %s"),
                operator ? "operator" : "function",
                tb_get(tb));
   }

   // Pure function may not call an impure function
   tree_t sub = top_scope->subprog;
   if ((sub != NULL) && (tree_kind(sub) == T_FUNC_BODY)) {
      if ((tree_attr_int(sub, impure_i, 0) == 0)
          && (tree_attr_int(decl, impure_i, 0) == 1))
         sem_error(t, "pure function %s cannot call impure function %s",
                   istr(tree_ident(sub)), istr(tree_ident(decl)));
   }

   if (!sem_copy_default_args(t, decl))
      return false;

#if 0
   printf("pick: %s\n", sem_type_str(tree_type(decl)));
   fmt_loc(stdout, tree_loc(t));
#endif

   tree_set_ref(t, decl);
   tree_set_type(t, type_result(tree_type(decl)));
   return true;
}

static bool sem_check_pcall(tree_t t)
{
   if (!sem_check_params(t))
      return false;

   int n_overloads = 0;
   int max_overloads = 128;
   tree_t *overloads LOCAL = xmalloc(max_overloads * sizeof(tree_t));

   tree_t decl;
   int n = 0, found_proc = 0;
   do {
      if ((decl = scope_find_nth(tree_ident2(t), n++))) {
         switch (tree_kind(decl)) {
         case T_PROC_DECL:
         case T_PROC_BODY:
            found_proc++;
            break;
         case T_ALIAS:
            if (tree_has_type(decl) && type_kind(tree_type(decl)) == T_PROC) {
               decl = tree_ref(tree_value(decl));
               found_proc++;
               break;
            }
            // Fall-through
         default:
            continue;   // Look for the next matching name
         }

         // Number of arguments must match
         if (!sem_check_arity(t, decl))
            continue;

         // Found a matching function definition
         ARRAY_APPEND(overloads, decl, n_overloads, max_overloads);
      }
   } while (decl != NULL);

   if (n_overloads == 0)
      sem_error(t, (found_proc > 0
                    ? "no matching procedure %s"
                    : "undefined procedure %s"),
                istr(tree_ident2(t)));

   int matches;
   if (!sem_resolve_overload(t, &decl, &matches, overloads, n_overloads))
      return false;

   if (matches > 0 && decl == NULL)
      return true;   // Resolved to a builtin function

   if (matches > 1) {
      LOCAL_TEXT_BUF tb = tb_new();

      for (int n = 0; n < n_overloads; n++) {
         if (overloads[n] != NULL)
            tb_printf(tb, "\n    %s",
                      sem_type_str(tree_type(overloads[n])));
      }

      sem_error(t, "ambiguous call to procedure %s%s",
                istr(tree_ident2(t)), tb_get(tb));
   }

   if (decl == NULL) {
      LOCAL_TEXT_BUF tb = tb_new();

      const char *fname = istr(tree_ident2(t));

      tb_printf(tb, "%s [", fname);
      const int nparams = tree_params(t);
      for (int i = 0; i < nparams; i++)
         tb_printf(tb, "%s%s",
                   (i == 0 ? "" : ", "),
                   sem_type_str(tree_type(tree_value(tree_param(t, i)))));
      tb_printf(tb, "]");

      sem_error(t, (n == 1 ? "undefined procedure %s"
                    : "no suitable overload for procedure %s"),
                tb_get(tb));
   }

   const int nparams = tree_params(t);
   const int nports  = tree_ports(decl);
   for (int i = 0; i < nparams; i++) {
      tree_t param = tree_param(t, i);

      int index = -1;
      if (tree_subkind(param) == P_POS)
         index = i;
      else {
         assert(tree_subkind(param) == P_NAMED);

         tree_t ref = tree_name(param);
         assert(tree_kind(ref) == T_REF);

         ident_t name = tree_ident(ref);
         for (int j = 0; (j < nports) && (index == -1); j++) {
            if (tree_ident(tree_port(decl, j)) == name)
               index = j;
         }
         assert(index != -1);

         tree_set_ref(ref, tree_port(decl, index));
      }

      tree_t  port     = tree_port(decl, index);
      class_t class    = tree_class(port);
      port_mode_t mode = tree_subkind(port);

      tree_t value = tree_value(param);
      tree_kind_t kind = tree_kind(value);
      while ((kind == T_ARRAY_REF) || (kind == T_ARRAY_SLICE)
             || (kind == T_ALL) || (kind == T_RECORD_REF)) {
         value = tree_value(value);
         kind  = tree_kind(value);
      }

      if (class == C_VARIABLE) {
         if (kind != T_REF)
            sem_error(value, "cannot associate this expression with "
                      "parameter class VARIABLE");

         tree_t decl = tree_ref(value);
         tree_kind_t decl_kind = tree_kind(decl);

         if (decl_kind == T_SIGNAL_DECL)
            sem_error(value, "cannot associate signal %s with parameter "
                      "class VARIABLE", istr(tree_ident(decl)));
         else if (decl_kind == T_FILE_DECL)
            sem_error(value, "cannot associate file %s with parameter "
                      "class VARIABLE", istr(tree_ident(decl)));
         else if (decl_kind == T_PORT_DECL) {
            const class_t class = tree_class(decl);
            if (mode == PORT_OUT && tree_subkind(decl) == PORT_IN)
               sem_error(value, "cannot read parameter %s with mode IN",
                         istr(tree_ident(decl)));
            else if ((mode == PORT_OUT || mode == PORT_INOUT)
                     && (class == C_CONSTANT || class == C_DEFAULT))
               sem_error(value, "object %s has class CONSTANT and "
                         "cannot be associated with OUT or INOUT parameters",
                         istr(tree_ident(decl)));
         }
         else if ((decl_kind != T_VAR_DECL) && (decl_kind != T_ALIAS))
            sem_error(value, "invalid use of name %s", istr(tree_ident(decl)));
      }
   }

   if (!sem_copy_default_args(t, decl))
      return false;

#if 0
   printf("pick: %s\n", sem_type_str(tree_type(decl)));
   fmt_loc(stdout, tree_loc(t));
#endif

   tree_set_ref(t, decl);
   return true;
}

static bool sem_check_wait(tree_t t)
{
   if (tree_has_delay(t)) {
      type_t std_time = sem_std_type("TIME");
      tree_t delay = tree_delay(t);

      if (!sem_check_constrained(delay, std_time))
         return false;

      if (!type_eq(tree_type(delay), std_time))
         sem_error(delay, "type of delay must be TIME");
   }

   if (tree_has_value(t)) {
      type_t std_bool = sem_std_type("BOOLEAN");
      tree_t value = tree_value(t);

      if (!sem_check_constrained(value, std_bool))
         return false;

      if (!type_eq(tree_type(value), std_bool))
         sem_error(value, "type of condition must be BOOLEAN");
   }

   return sem_check_sensitivity(t);
}

static bool sem_check_assert(tree_t t)
{
   // Rules for asserion statements are in LRM 93 section 8.2

   type_t std_bool     = sem_std_type("BOOLEAN");
   type_t std_string   = sem_std_type("STRING");
   type_t std_severity = sem_std_type("SEVERITY_LEVEL");

   tree_t value    = tree_value(t);
   tree_t severity = tree_severity(t);
   tree_t message  = tree_has_message(t) ? tree_message(t) : NULL;

   if (!sem_check_constrained(value, std_bool))
      return false;

   if (!sem_check_constrained(severity, std_severity))
      return false;

   if (message != NULL) {
      if (!sem_check_constrained(message, std_string))
         return false;
   }

   if (!type_eq(tree_type(value), std_bool))
      sem_error(value, "type of assertion expression must "
                "be %s but is %s", sem_type_str(std_bool),
                sem_type_str(tree_type(value)));

   if (!type_eq(tree_type(severity), std_severity))
      sem_error(severity, "type of severity must be %s but is %s",
                sem_type_str(std_severity),
                sem_type_str(tree_type(severity)));

   if (message != NULL) {
      if (!type_eq(tree_type(message), std_string))
         sem_error(message, "type of message be %s but is %s",
                   sem_type_str(std_string),
                   sem_type_str(tree_type(message)));
   }

   return true;
}

static tree_t sem_array_len(type_t type)
{
   range_t r = type_dim(type, 0);
   type_t index_type = tree_type(r.left);

   tree_t one = sem_int_lit(index_type, 1);

   tree_t tmp;
   if (r.kind == RANGE_TO)
      tmp = call_builtin("sub", index_type, r.right, r.left, NULL);
   else
      tmp = call_builtin("sub", index_type, r.left, r.right, NULL);

   return call_builtin("add", index_type, tmp, one, NULL);
}

static bool sem_check_concat_param(tree_t t, type_t hint)
{
   type_set_t *old = top_type_set;

   type_set_push();

   for (unsigned i = 0; i < old->n_members; i++) {
      if (!type_is_array(old->members[i]))
         continue;

      type_t base = type_base_recur(old->members[i]);
      type_t elem = type_elem(base);

      if (hint == NULL) {
         if (type_is_unconstrained(base))
            type_set_add(base);

         type_set_add(elem);
      }
      else if (!type_is_array(hint)) {
         if (type_eq(type_elem(base), hint)) {
            type_set_add(base);
            type_set_add(elem);
         }
      }
   }

   if (hint != NULL) {
      type_t base = type_base_recur(hint);
      if (type_is_unconstrained(base))
         type_set_add(base);

      if (type_is_array(hint))
         type_set_add(type_elem(hint));
   }

   bool ok = sem_check(t);
   type_set_pop();

   return ok;
}

static bool sem_is_composite(type_t t)
{
   return type_is_array(t) || type_is_record(t);
}

static bool sem_check_concat(tree_t t)
{
   // Concatenation expressions are treated differently to other operators
   // as they have special rules. See LRM 93 section 7.2.4

   assert(tree_params(t) == 2);
   tree_t left  = tree_value(tree_param(t, 0));
   tree_t right = tree_value(tree_param(t, 1));

   if ((top_type_set->n_members > 0) && !type_set_restrict(sem_is_composite)) {
      LOCAL_TEXT_BUF ts = type_set_fmt();
      sem_error(t, "no composite type in context%s", tb_get(ts));
   }

   if (sem_ambiguous_rate(left) < sem_ambiguous_rate(right)) {
      if (!sem_check_concat_param(left, NULL)
          || !sem_check_concat_param(right, tree_type(left)))
         return false;
   }
   else {
      if (!sem_check_concat_param(right, NULL)
          || !sem_check_concat_param(left, tree_type(right)))
         return false;
   }

   type_t ltype = tree_type(left);
   type_t rtype = tree_type(right);

   type_kind_t lkind = type_kind(ltype);
   type_kind_t rkind = type_kind(rtype);

   bool l_array = type_is_array(ltype);
   bool r_array = type_is_array(rtype);

   if (l_array && r_array) {
      const bool l_elem_array = type_is_array(type_elem(ltype));
      const bool r_elem_array = type_is_array(type_elem(rtype));

      if (l_elem_array && !r_elem_array)
         r_array = false;
      else if (!l_elem_array && r_elem_array)
         l_array = false;
   }

   if (l_array && r_array) {
      if (!type_eq(ltype, rtype))
         sem_error(t, "cannot concatenate arrays of types %s and %s",
                   sem_type_str(ltype), sem_type_str(rtype));

      if (array_dimension(ltype) > 1)
         sem_error(t, "cannot concatenate arrays with more than one dimension");

      type_t index_type = index_type_of(ltype, 0);
      range_t index_r = type_dim(index_type, 0);

      if ((lkind == T_UARRAY) || (rkind == T_UARRAY))
         tree_set_type(t, (lkind == T_UARRAY) ? ltype : rtype);
      else {
         tree_t left_len = sem_array_len(ltype);
         tree_t right_len = sem_array_len(rtype);

         type_t result = type_new(T_SUBTYPE);
         type_set_ident(result, type_ident(ltype));
         type_set_base(result, ltype);

         tree_t one = sem_int_lit(index_type, 1);

         tree_t result_len = call_builtin(
            "add", index_type, left_len, right_len, NULL);
         tree_t tmp = call_builtin(
            "add", index_type, result_len, index_r.left, NULL);
         tree_t result_right = call_builtin(
            "sub", index_type, tmp, one, NULL);

         range_t result_r = {
            .kind  = index_r.kind,
            .left  = index_r.left,
            .right = result_right
         };
         type_add_dim(result, result_r);

         tree_set_type(t, result);
      }
   }
   else if (r_array || l_array) {
      tree_t array  = (l_array ? left : right);
      tree_t scalar = (l_array ? right : left);

      type_t atype = tree_type(array);
      type_t stype = tree_type(scalar);

      type_kind_t akind = type_kind(atype);

      if (array_dimension(atype) > 1)
         sem_error(t, "cannot concatenate arrays with more than one dimension");

      if (!type_eq(stype, type_elem(atype)))
         sem_error(t, "type of scalar does not match element type of array");

      type_t index_type = index_type_of(atype, 0);
      range_t index_r = type_dim(index_type, 0);

      type_t std_int = sem_std_type("INTEGER");
      tree_t array_len;
      if (akind == T_UARRAY)
         array_len = call_builtin("length", std_int,
                                  sem_int_lit(std_int, 1), array, NULL);
      else
         array_len = sem_array_len(atype);

      tree_t result_right = call_builtin(
         "add", index_type, index_r.left, array_len, NULL);

      type_t result = type_new(T_SUBTYPE);
      type_set_ident(result, type_ident(atype));
      type_set_base(result, atype);

      range_t result_r = {
         .kind  = index_r.kind,
         .left  = index_r.left,
         .right = result_right
      };
      type_add_dim(result, result_r);

      tree_set_type(t, result);
   }
   else {
      // Concatenating two scalars

      if (!type_eq(ltype, rtype))
         sem_error(t, "cannot concatenate values of different types");

      // Match the element type to a composite in the type set
      type_t composite = NULL;
      for (unsigned i = 0; i < top_type_set->n_members; i++) {
         type_t this = top_type_set->members[i];
         if (type_is_array(this) && type_eq(type_elem(this), ltype))
            composite = this;
      }
      assert(composite != NULL);

      type_t index_type = index_type_of(composite, 0);
      range_t index_r = type_dim(index_type, 0);

      tree_t result_right = call_builtin(
         "add", index_type, index_r.left, sem_int_lit(index_type, 1), NULL);

      type_t result = type_new(T_SUBTYPE);
      type_set_ident(result, type_ident(composite));
      type_set_base(result, composite);

      range_t result_r = {
         .kind  = index_r.kind,
         .left  = index_r.left,
         .right = result_right
      };
      type_add_dim(result, result_r);

      tree_set_type(t, result);
   }

   return true;
}

static bool sem_is_character_array(type_t t)
{
   // According LRM 93 section 3.1.1 an enumeration type is a character
   // type if at least one of its enumeration literals is a character
   // literal

   if (!type_is_array(t))
      return false;

   if (array_dimension(t) != 1)
      return false;

   type_t elem = type_base_recur(type_elem(t));

   if (!type_is_enum(elem))
      return false;

   const int nlits = type_enum_literals(elem);
   for (int i = 0; i < nlits; i++) {
      tree_t lit = type_enum_literal(elem, i);
      if (ident_char(tree_ident(lit), 0) == '\'')
         return true;
   }

   return false;
}

static bool sem_check_string_literal(tree_t t)
{
   // String literals are in LRM 93 section 7.3.1

   // The type must be determinable soley from the context excluding the
   // literal itself but using the fact that the type must be a one
   // dimensional array of a character type

   if (!type_set_restrict(sem_is_character_array))
      sem_error(t, "no one dimensional arrays of character type in context");

   type_t type;
   if (!type_set_uniq(&type)) {
      LOCAL_TEXT_BUF ts = type_set_fmt();
      sem_error(t, "type of string literal is ambiguous%s", tb_get(ts));
   }

   type_t elem = type_base_recur(type_elem(type));

   const int nlits = type_enum_literals(elem);
   const int nchars = tree_chars(t);
   for (int i = 0; i < nchars; i++) {
      tree_t ch = tree_char(t, i);

      ident_t ch_i = tree_ident(ch);
      bool valid = false;
      for (int j = 0; !valid && (j < nlits); j++) {
         tree_t lit = type_enum_literal(elem, j);
         if (ch_i == tree_ident(lit)) {
            tree_set_ref(ch, lit);
            tree_set_type(ch, elem);
            valid = true;
         }
      }

      if (!valid)
         sem_error(t, "invalid character %s in string literal of type %s",
                   istr(ch_i), sem_type_str(type));
   }

   if (type_is_unconstrained(type)) {
      // Construct a new array type: the direction and bounds are the same
      // as those for a positional array aggregate

      type_t tmp = type_new(T_SUBTYPE);
      type_set_ident(tmp, type_ident(type));
      type_set_base(tmp, type);

      type_t index_type = index_type_of(type, 0);

      // The direction is determined by the index type
      range_kind_t dir;
      if (type_kind(index_type) == T_ENUM)
         dir = RANGE_TO;
      else
         dir = type_dim(index_type, 0).kind;

      // The left bound is the left of the index type and the right bound
      // is determined by the number of elements

      tree_t left = NULL, right = NULL;
      type_t std_int = sem_std_type("INTEGER");

      if (type_kind(index_type) == T_ENUM)
         left = make_ref(type_enum_literal(index_type, 0));
      else
         left = type_dim(index_type, 0).left;

      right = call_builtin("add", index_type,
                           sem_int_lit(std_int, nchars - 1),
                           left, NULL);

      range_t r = {
         .kind  = dir,
         .left  = left,
         .right = right
      };
      type_add_dim(tmp, r);

      tree_set_type(t, tmp);
      tree_add_attr_int(t, unconstrained_i, 1);
   }
   else
      tree_set_type(t, type);

   return true;
}

static bool sem_check_literal(tree_t t)
{
   if (tree_has_type(t) && (type_kind(tree_type(t)) != T_UNRESOLVED))
      return true;

   switch (tree_subkind(t)) {
   case L_INT:
      tree_set_type(t, type_universal_int());
      break;

   case L_REAL:
      tree_set_type(t, type_universal_real());
      break;

   case L_NULL:
      {
         type_t access_type;
         if (!type_set_uniq(&access_type))
            sem_error(t, "invalid use of null expression");

         if (type_kind(access_type) != T_ACCESS)
            sem_error(t, "null expression must have access type");

         tree_set_type(t, access_type);
      }
      break;

   case L_STRING:
      return sem_check_string_literal(t);

   default:
      assert(false);
   }

   return true;
}

static bool sem_check_aggregate(tree_t t)
{
   // Rules for aggregates are in LRM 93 section 7.3.2

   // The type of an aggregate must be determinable solely from the
   // context in which the aggregate appears

   if (!type_set_restrict(sem_is_composite)) {
      LOCAL_TEXT_BUF ts = type_set_fmt();
      sem_error(t, "no composite type in context%s", tb_get(ts));
   }

   type_t composite_type;
   if (!type_set_uniq(&composite_type)) {
      LOCAL_TEXT_BUF ts = type_set_fmt();
      sem_error(t, "type of aggregate is ambiguous%s", tb_get(ts));
   }

   type_t base_type = composite_type;
   while (type_kind(base_type) == T_SUBTYPE)
      base_type = type_base(base_type);

   const bool unconstrained = type_is_unconstrained(composite_type);
   const bool array = type_is_array(composite_type);

   // All positional associations must appear before named associations
   // and those must appear before any others association

   enum { POS, NAMED, OTHERS } state = POS;
   bool have_named = false;
   bool have_pos = false;
   bool have_others = false;

   const int nassocs = tree_assocs(t);

   for (int i = 0; i < nassocs; i++) {
      tree_t a = tree_assoc(t, i);

      switch (tree_subkind(a)) {
      case A_POS:
         if (state > POS)
            sem_error(a, "positional associations must appear "
                      "first in aggregate");
         have_pos = true;
         break;

      case A_NAMED:
      case A_RANGE:
         if (state > NAMED)
            sem_error(a, "named association must not follow "
                      "others association in aggregate");
         state = NAMED;
         have_named = true;
         break;

      case A_OTHERS:
         if (state == OTHERS)
            sem_error(a, "only a single others association "
                      "allowed in aggregate");
         if (unconstrained)
            sem_error(a, "others choice not allowed in this context");
         state = OTHERS;
         have_others = true;
         break;
      }
   }

   // Named and positional associations cannot be mixed in array
   // aggregates

   if (array && have_named && have_pos)
      sem_error(t, "named and positional associations cannot be "
                "mixed in array aggregates");

   // All elements must be of the composite base type if this is
   // a one-dimensional array otherwise construct an array type
   // with n-1 dimensions.

   if (array) {
      type_t elem_type = NULL;
      const int ndims = array_dimension(composite_type);
      if (ndims == 1)
         elem_type = type_elem(base_type);
      else
         elem_type = array_aggregate_type(composite_type, 1);

      type_t index_type = index_type_of(composite_type, 0);

      for (int i = 0; i < nassocs; i++) {
         tree_t a = tree_assoc(t, i);

         switch (tree_subkind(a)) {
         case A_RANGE:
            {
               range_t r = tree_range(a);
               if (!sem_check_range(&r, index_type))
                  return false;
               tree_set_range(a, r);
            }
            break;

         case A_NAMED:
            if (!sem_check_constrained(tree_name(a), index_type))
               return false;
            break;

         default:
            break;
         }

         tree_t value = tree_value(a);

         if (!sem_check_constrained(value, elem_type))
            return false;

         if (!type_eq(elem_type, tree_type(value)))
            sem_error(value, "type of element %s does not match base "
                      "type of aggregate %s",
                      sem_type_str(tree_type(value)),
                      sem_type_str(elem_type));
      }
   }

   // Checks for record aggregates are given in LRM 93 section 7.3.2.1

   if (type_is_record(base_type)) {
      const int nfields = type_fields(base_type);
      bool have[nfields];
      int pos = 0;
      for (int i = 0; i < nfields; i++)
         have[i] = false;

      for (int i = 0; i < nassocs; i++) {
         tree_t a = tree_assoc(t, i);
         int f = -1;

         switch (tree_subkind(a)) {
         case A_NAMED:
            {
               tree_t name = tree_name(a);
               if (tree_kind(name) != T_REF)
                  sem_error(name, "association name must be a field "
                            "identifier");

               ident_t name_i = tree_ident(name);
               for (f = 0; f < nfields; f++) {
                  tree_t field = type_field(base_type, f);
                  if (tree_ident(field) == name_i) {
                     tree_set_type(name, tree_type(field));
                     tree_set_ref(name, field);
                     break;
                  }
               }

               if (f == nfields)
                  sem_error(name, "type %s does not have field named %s",
                            sem_type_str(composite_type), istr(name_i));
            }
            break;

         case A_POS:
            {
               if (pos >= nfields)
                  sem_error(t, "too many positional associations");

               f = pos++;
            }
            break;

         case A_OTHERS:
            f = -1;
            break;

         case A_RANGE:
            sem_error(a, "range is not allowed here");
         }

         for (int j = 0; j < nfields; j++) {
            if ((f != -1) && (f != j))
               continue;

            tree_t field = type_field(base_type, j);
            type_t field_type = tree_type(field);

            if (have[j]) {
               if (f == -1)
                  continue;
               else
                  sem_error(a, "field %s already has a value",
                            istr(tree_ident(field)));
            }

            tree_t value = tree_value(a);

            if (!sem_check_constrained(value, field_type))
               return false;

            type_t value_type = tree_type(value);
            if (!type_eq(field_type, value_type))
               sem_error(a, "type of value %s does not match type "
                         "of field %s %s",
                         sem_type_str(value_type),
                         istr(tree_ident(field)),
                         sem_type_str(field_type));

            if (type_is_universal(value_type))
               tree_set_type(value, field_type);

            have[j] = true;
         }
      }

      for (int i = 0; i < nfields; i++) {
         if (!have[i]) {
            tree_t field = type_field(base_type, i);
            sem_error(t, "field %s does not have a value",
                      istr(tree_ident(field)));
         }
      }
   }

   // If a named choice is not locally static then it must be the
   // only element

   for (int i = 0; i < nassocs; i++) {
      tree_t a = tree_assoc(t, i);
      if (tree_subkind(a) == A_NAMED && !sem_locally_static(tree_name(a))) {
         if (tree_assocs(t) != 1)
            sem_error(tree_name(a), "non-locally static choice must be "
                      "only choice");
      }
   }

   // If there is no others choice or the base type is unconstrained then
   // construct a new array subtype using the rules in LRM 93 7.3.2.2

   if (array && (have_named || unconstrained) && !have_others) {
      type_t tmp = type_new(T_SUBTYPE);
      type_set_ident(tmp, type_ident(base_type));
      type_set_base(tmp, base_type);

      const int ndims = array_dimension(composite_type);

      type_t index_type = index_type_of(composite_type, 0);

      range_kind_t dir;
      if (unconstrained) {
         // The direction is determined by the index type
         if (type_kind(index_type) == T_ENUM)
            dir = RANGE_TO;
         else
            dir = type_dim(index_type, 0).kind;
      }
      else {
         // The direction is determined by the context
         dir = type_dim(composite_type, 0).kind;
      }

      tree_t left = NULL, right = NULL;

      if (have_pos || !have_named) {
         // The left bound is the left of the index type and the right bound
         // is determined by the number of elements

         assert(unconstrained);

         type_t std_int = sem_std_type("INTEGER");

         if (type_kind(index_type) == T_ENUM)
            left = make_ref(type_enum_literal(index_type, 0));
         else
            left = type_dim(index_type, 0).left;

         right = call_builtin("add", index_type,
                              sem_int_lit(std_int, nassocs - 1),
                              left, NULL);
      }
      else {
         // The left and right bounds are determined by the smallest and
         // largest choices

         tree_t low  = call_builtin("min", index_type, NULL);
         tree_t high = call_builtin("max", index_type, NULL);

         tree_set_loc(low, tree_loc(t));
         tree_set_loc(high, tree_loc(t));

         for (int i = 0; i < nassocs; i++) {
            tree_t a = tree_assoc(t, i);
            switch (tree_subkind(a)) {
            case A_NAMED:
               {
                  tree_t name = tree_name(a);
                  add_param(low, name, P_POS, NULL);
                  add_param(high, name, P_POS, NULL);
               }
               break;

            case A_RANGE:
               {
                  range_t r = tree_range(a);
                  if (r.kind == RANGE_TO) {
                     add_param(low, r.left, P_POS, NULL);
                     add_param(high, r.right, P_POS, NULL);
                  }
                  else if (r.kind == RANGE_DOWNTO) {
                     add_param(low, r.right, P_POS, NULL);
                     add_param(high, r.left, P_POS, NULL);
                  }
                  else {
                     add_param(low, r.right, P_POS, NULL);
                     add_param(low, r.left, P_POS, NULL);
                     add_param(high, r.right, P_POS, NULL);
                     add_param(high, r.left, P_POS, NULL);
                  }
               }
               break;
            }
         }

         left  = (dir == RANGE_TO ? low : high);
         right = (dir == RANGE_TO ? high : low);
      }

      range_t r = {
         .kind  = dir,
         .left  = left,
         .right = right
      };
      type_add_dim(tmp, r);

      for (int i = 1; i < ndims; i++) {
         range_t dim;
         if (unconstrained)
            dim = type_dim(tree_type(tree_value(tree_assoc(t, 0))), i - 1);
         else
            dim = type_dim(composite_type, i);
         type_add_dim(tmp, dim);
      }

      tree_set_type(t, tmp);
   }
   else
      tree_set_type(t, composite_type);

   if (unconstrained)
      tree_add_attr_int(t, unconstrained_i, 1);

   return true;
}

static void sem_convert_to_record_ref(tree_t t, tree_t decl)
{
   // Convert an ordinary reference to a record reference
   // The prefix of the name must be the record
   ident_t base = ident_runtil(tree_ident(t), '.');
   tree_t rec = scope_find(base);
   assert(rec != NULL);

   tree_t value = NULL;
   const loc_t *loc = tree_loc(t);

   type_t rec_type = tree_type(rec);
   if (type_kind(rec_type) == T_ACCESS) {
      // Record fields can be dereferenced implicitly
      value = tree_new(T_ALL);
      tree_set_loc(value, loc);
      tree_set_value(value, make_ref(rec));
      tree_set_type(value, type_access(tree_type(rec)));
   }
   else
      assert(type_is_record(rec_type));

   if (tree_kind(rec) == T_FIELD_DECL) {
      value = tree_new(T_REF);
      tree_set_loc(value, loc);
      tree_set_ident(value, base);
      sem_convert_to_record_ref(value, rec);
   }
   else if (value == NULL) {
      value = make_ref(rec);
      tree_set_loc(value, loc);
   }

   tree_change_kind(t, T_RECORD_REF);
   tree_set_value(t, value);
   tree_set_ident(t, tree_ident(decl));
   tree_set_type(t, tree_type(decl));
}

static bool sem_check_pure_ref(tree_t ref, tree_t decl)
{
   const bool is_pure_func =
      top_scope->subprog != NULL
      && tree_kind(top_scope->subprog) == T_FUNC_BODY
      && tree_attr_int(top_scope->subprog, impure_i, 0) == 0;

   if (is_pure_func) {
      scope_t *owner = scope_containing(top_scope, decl);
      if (owner != NULL && owner->subprog != top_scope->subprog)
         sem_error(ref, "invalid reference to %s inside pure function %s",
                   istr(tree_ident(decl)),
                   istr(tree_ident(top_scope->subprog)));
   }

   return true;
}

static bool sem_check_ref(tree_t t)
{
   tree_t decl = NULL, next;
   ident_t name = tree_ident(t);
   int n = 0;
   do {
      if ((next = scope_find_nth(name, n))) {
         class_t class = class_of(next);
         if ((class == C_ENTITY) || (class == C_ARCHITECTURE)
             || (class == C_COMPONENT) || (class == C_LABEL))
            continue;

         type_t type = tree_type(next);

         const bool zero_arg_fn =
            (type_kind(type) == T_FUNC)
            && (sem_required_args(next) == 0)
            && type_set_member(type_result(type));

         if (type_set_member(type) || zero_arg_fn) {
            if (decl != NULL) {
               assert(zero_arg_fn || (tree_kind(next) == T_ENUM_LIT));
               sem_error(t, "ambiguous %s %s",
                         (tree_kind(next) != tree_kind(decl))
                         ? "use of name"
                         : ((zero_arg_fn
                             ? "call to function"
                             : "use of enumeration literal")),
                         istr(name));
            }
            decl = next;
         }

         n++;
      }
   } while ((next != NULL) && scope_can_overload(next));

   if (decl == NULL)
      decl = next;

   if (decl == NULL) {
      if (n == 0)
         sem_error(t, "no visible declaration for %s", istr(name));
      else if (n == 1) {
         LOCAL_TEXT_BUF ts = type_set_fmt();
         sem_error(t, "name %s cannot be used in this context%s",
                   istr(name), tb_get(ts));
      }
      else {
         LOCAL_TEXT_BUF ts = type_set_fmt();
         sem_error(t, "no suitable overload for identifier %s in "
                   "context%s", istr(name), tb_get(ts));
      }
   }

   switch (tree_kind(decl)) {
   case T_PORT_DECL:
      if (tree_class(decl) != C_CONSTANT) {
      case T_VAR_DECL:
      case T_SIGNAL_DECL:
         if (!sem_check_pure_ref(t, decl))
            return false;
      }
      // Fall-through
   case T_CONST_DECL:
   case T_ENUM_LIT:
   case T_ALIAS:
   case T_FILE_DECL:
   case T_UNIT_DECL:
   case T_GENVAR:
      tree_set_type(t, tree_type(decl));
      break;

   case T_FUNC_DECL:
   case T_FUNC_BODY:
      tree_change_kind(t, T_FCALL);
      tree_set_type(t, type_result(tree_type(decl)));
      sem_copy_default_args(t, decl);
      break;

   case T_FIELD_DECL:
      sem_convert_to_record_ref(t, decl);
      return sem_check(t);

   default:
      sem_error(t, "invalid use of %s", istr(tree_ident(t)));
   }

   tree_set_ref(t, decl);
   return true;
}

static tree_t sem_find_record_field(tree_t rref)
{
   ident_t fname = tree_ident(rref);
   type_t value_type = tree_type(tree_value(rref));

   const int nfields = type_fields(value_type);
   for (int i = 0; i < nfields; i++) {
      tree_t field = type_field(value_type, i);
      if (tree_ident(field) == fname)
         return field;
   }

   return NULL;
}

static bool sem_check_record_ref(tree_t t)
{
   tree_t value = tree_value(t);
   if (!sem_check_constrained(value, NULL))
      return false;

   type_t value_type = tree_type(value);
   if (!type_is_record(value_type))
      sem_error(value, "expected record type but found %s",
                sem_type_str(value_type));

   tree_t field = sem_find_record_field(t);
   if (field == NULL)
      sem_error(t, "record type %s has no field %s",
                sem_type_str(value_type), istr(tree_ident(t)));

   tree_set_type(t, tree_type(field));
   return true;
}

static type_t sem_implicit_dereference(tree_t t, get_fn_t get, set_fn_t set)
{
   // Construct the implicit dereference when slicing or indexing arrays
   // through accesses

   tree_t value = get(t);

   type_t type = tree_type(value);
   assert(type_kind(type) == T_ACCESS);

   type_t access = type_access(type);

   tree_t all = tree_new(T_ALL);
   tree_set_loc(all, tree_loc(value));
   tree_set_value(all, value);
   tree_set_type(all, access);

   set(t, all);

   return access;
}

static bool sem_check_array_ref(tree_t t)
{
   tree_t value = tree_value(t);
   if (!sem_check_constrained(value, NULL))
      return false;

   type_t type = tree_type(tree_value(t));

   if (type_kind(type) == T_ACCESS)
      type = sem_implicit_dereference(t, tree_value, tree_set_value);

   if (!type_is_array(type))
      sem_error(t, "cannot index non-array type %s", sem_type_str(type));

   const int nindex  = array_dimension(type);
   const int nparams = tree_params(t);

   if (nparams != nindex)
      sem_error(t, "array %s has %d dimensions but %d indices given",
                istr(tree_ident(value)), nindex, nparams);

   bool ok = true;
   for (int i = 0; i < nparams; i++) {
      tree_t p = tree_param(t, i);
      if (tree_subkind(p) != P_POS)
         sem_error(t, "only scalar references supported");

      type_t expect = index_type_of(type, i);

      tree_t value = tree_value(p);

      if (tree_kind(value) == T_REF) {
         // Handle slices using a subtype as a range
         tree_t decl = scope_find(tree_ident(value));
         if ((decl != NULL) && (tree_kind(decl) == T_TYPE_DECL)) {
            tree_change_kind(t, T_ARRAY_SLICE);
            tree_set_range(t, type_dim(tree_type(decl), 0));

            return sem_check(t);
         }
      }

      ok = sem_check_constrained(value, expect) && ok;

      if (ok && !type_eq(expect, tree_type(value)))
         sem_error(value, "type of index %s does not match type of "
                   "array dimension %s",
                   sem_type_str(tree_type(value)),
                   sem_type_str(expect));
   }

   tree_set_type(t, type_elem(type));
   return ok;
}

static bool sem_check_array_slice(tree_t t)
{
   if (!sem_check(tree_value(t)))
      return false;

   type_t array_type = tree_type(tree_value(t));

   if (type_kind(array_type) == T_ACCESS)
      array_type = sem_implicit_dereference(t, tree_value, tree_set_value);

   if (!type_is_array(array_type))
      sem_error(t, "type of slice prefix is not an array");

   range_t r = tree_range(t);
   if (!sem_check_range(&r, index_type_of(array_type, 0)))
      return false;

   tree_set_range(t, r);

   bool wrong_dir =
      (type_kind(array_type) != T_UARRAY)
      && (r.kind != type_dim(array_type, 0).kind)
      && (type_dim(array_type, 0).kind != RANGE_DYN);

   if (wrong_dir)
      sem_error(t, "range direction of slice does not match prefix");

   type_t slice_type = type_new(T_SUBTYPE);
   type_set_ident(slice_type, type_ident(array_type));
   type_set_base(slice_type, array_type);
   type_add_dim(slice_type, tree_range(t));

   tree_set_type(t, slice_type);
   return true;
}

static bool sem_check_attr_ref(tree_t t)
{
   // Attribute names are in LRM 93 section 6.6

   bool special = false;
   tree_t name = tree_name(t), decl = NULL;
   if ((tree_kind(name) == T_REF) && (decl = scope_find(tree_ident(name)))) {
      tree_kind_t kind = tree_kind(decl);
      if (kind == T_TYPE_DECL) {
         // Special case for attributes of types
         tree_set_ref(name, decl);
         tree_set_type(name, tree_type(decl));

         special = true;
      }
      else {
         class_t class = class_of(decl);
         if (!class_has_type(class) || (class == C_PROCEDURE)
             || (class == C_FUNCTION)) {
            // Special case for attributes of entities, architectures, etc.
            tree_set_ref(name, decl);

            special = true;
         }
      }
   }

   if (!special && !sem_check_constrained(name, NULL))
      return false;

   if (tree_has_type(name) && (type_kind(tree_type(name)) == T_ACCESS)) {
      // Convert implicit dereference such as PTR'X to PTR.ALL'X
      sem_implicit_dereference(t, tree_name, tree_set_name);
      name = tree_name(t);
   }

   ident_t attr = tree_ident(t);

   if (icmp(attr, "LAST_EVENT")) {
      if (class_of(name) != C_SIGNAL)
         sem_error(t, "prefix of attribute %s must denote a signal",
                   istr(attr));

      tree_set_type(t, sem_std_type("TIME"));
      return true;
   }

   if (!sem_static_name(name))
      sem_error(name, "invalid attribute reference");

   bool allow_user = true;
   tree_t search = name;
   while (decl == NULL) {
      switch (tree_kind(search)) {
      case T_REF:
         decl = tree_ref(search);
         break;

      case T_RECORD_REF:
         decl = sem_find_record_field(search);
         assert(decl != NULL);
         break;

      default:
         search = tree_value(search);
         allow_user = false;   // LRM disallows user-defined attributes
                               // where prefix is slice or sub-element
      }
   }

   if (icmp(attr, "range"))
      sem_error(t, "range expression not allowed here");

   if (top_scope->subprog != NULL) {
      // The following attributes are illegal inside a subprogram
      // according to LRM 93 section 2.1.1.2
      ident_t illegal[] = {
         ident_new("DELAYED"),
         ident_new("STABLE"),
         ident_new("QUIET"),
         ident_new("TRANSACTION")
      };

      for (int i = 0; i < ARRAY_LEN(illegal); i++) {
         if (illegal[i] == attr)
            sem_error(t, "implicit signal %s cannot be used in a "
                      "subprogram body", istr(attr));
      }
   }

   tree_t a = tree_attr_tree(decl, attr);
   if (a == NULL)
      sem_error(t, "object %s has no attribute %s",
                istr(tree_ident(decl)), istr(attr));

   if (tree_kind(a) == T_FUNC_DECL) {
      type_t ftype = tree_type(a);

      char *buf LOCAL = xasprintf("_arg%d", tree_ports(a) - 1);
      ident_t pname = ident_new(buf);

      // For an expression X'A(..) add X as a final parameter
      bool already_added = false;
      for (int i = 0; (i < tree_params(t)) && !already_added; i++) {
         tree_t p = tree_param(t, i);
         if ((tree_subkind(p) == P_NAMED)
             && (tree_ident(tree_name(p)) == pname))
            already_added = true;
      }

      if (!already_added)
         add_param(t, name, P_NAMED, make_ref(tree_port(a, tree_ports(a) - 1)));

      sem_copy_default_args(t, a);

      const int nports  = tree_ports(a);
      const int nparams = tree_params(t);

      if (nparams != type_params(ftype))
         sem_error(t, "expected %d parameters for attribute %s "
                   "but have %d", type_params(ftype), istr(attr), nparams);

      for (int i = 0; i < nparams; i++) {
         tree_t p = tree_param(t, i);

         int pindex = -1;
         if (tree_subkind(p) == P_POS)
            pindex = i;
         else {
            assert(tree_subkind(p) == P_NAMED);

            tree_t name = tree_name(p);
            assert(tree_kind(name) == T_REF);

            if (tree_ident(name) == pname)
               continue;

            for (int j = 0; (j < nports) && (pindex == -1); j++) {
               if (tree_ident(tree_port(a, j)) == tree_ident(name))
                  pindex = j;
            }
            assert(pindex != -1);
         }

         tree_t value = tree_value(p);

         type_t expect_type = type_param(ftype, pindex);
         if (!sem_check_constrained(value, expect_type))
            return false;

         if (!type_eq(tree_type(value), expect_type))
            sem_error(t, "expected type %s for attribute %s but found %s",
                      sem_type_str(expect_type), istr(attr),
                      sem_type_str(tree_type(value)));

         // Check cases where a dimension must be locally static
         tree_t port = tree_port(a, pindex);
         if (tree_attr_int(port, locally_static_i, 0)) {
            if (!sem_locally_static(value))
               sem_error(p, "parameter must be locally static");
         }
      }

      type_t result_type = type_result(ftype);
      if (type_kind(result_type) == T_NONE) {
         // This is a kludge to handle the result type of 'LEFT, etc. for
         // arrays being dependent on the dimension argument

         type_t array_type = tree_type(decl);
         if (type_kind(array_type) == T_ACCESS)
            array_type = type_access(array_type);
         assert(type_is_array(array_type));

         ident_t arg0_i = ident_new("_arg0");
         for (int i = 0; i < nparams; i++) {
            tree_t p = tree_param(t, i);
            if ((tree_subkind(p) != P_POS)
                && (tree_ident(tree_name(p)) != arg0_i))
               continue;

            tree_t pv = tree_value(p);
            if (tree_kind(pv) != T_LITERAL)
               sem_error(pv, "dimension argument must be a literal");
            else
               result_type = index_type_of(array_type, tree_ival(pv) - 1);
         }
      }

      tree_set_type(t, result_type);

      tree_set_ref(t, a);
   }
   else if (!allow_user)
      sem_error(t, "prefix of user defined attribute reference cannot "
                "denote a sub-element or slice of an object");
   else if (tree_params(t) > 0) {
      // This must be indexing an attribute with an array type
      tree_t new = tree_new(T_ATTR_REF);
      tree_set_name(new, tree_name(t));
      tree_set_ident(new, tree_ident(t));
      tree_set_loc(new, tree_loc(t));

      tree_change_kind(t, T_ARRAY_REF);
      tree_set_value(t, new);

      return sem_check(t);
   }
   else {
      tree_set_value(t, a);
      tree_set_type(t, tree_type(a));
      tree_set_ref(t, decl);
   }

   return true;
}

static bool sem_check_qualified(tree_t t)
{
   tree_t decl = scope_find(tree_ident(t));
   if (tree_kind(decl) != T_TYPE_DECL)
      sem_error(t, "%s is not a type name", istr(tree_ident(t)));

   type_t type = tree_type(decl);
   tree_set_type(t, type);
   return sem_check_constrained(tree_value(t), type);
}

static bool sem_check_actual(formal_map_t *formals, int nformals,
                             tree_t param, tree_t unit)
{
   tree_t value = tree_value(param);
   tree_t decl = NULL;
   type_t type = NULL;

   switch (tree_subkind(param)) {
   case P_POS:
      {
         const int pos = tree_pos(param);
         if (pos >= nformals)
            sem_error(value, "too many positional actuals");
         if (formals[pos].have)
            sem_error(value, "formal %s already has an actual",
                      istr(tree_ident(formals[pos].decl)));
         formals[pos].have = true;
         decl = formals[pos].decl;
         type = tree_type(decl);
      }
      break;

   case P_NAMED:
      {
         tree_t name = tree_name(param);
         tree_kind_t kind = tree_kind(name);
         tree_t ref = name;
         tree_t conv = NULL;

         if ((kind == T_FCALL) || (kind == T_TYPE_CONV)) {
            if (tree_params(name) != 1)
               sem_error(name, "output conversion function must have "
                         "exactly one parameter");

            conv = name;
            name = ref = tree_value(tree_param(name, 0));
            kind = tree_kind(ref);
         }

         while ((kind == T_ARRAY_REF) || (kind == T_ARRAY_SLICE)) {
            ref  = tree_value(ref);
            kind = tree_kind(ref);
         }

         assert(tree_kind(ref) == T_REF);

         for (int i = 0; i < nformals; i++) {
            if (tree_ident(formals[i].decl) == tree_ident(ref)) {
               if (formals[i].have && !formals[i].partial)
                  sem_error(value, "formal %s already has an actual",
                            istr(tree_ident(formals[i].decl)));
               formals[i].have    = true;
               formals[i].partial = (tree_kind(name) != T_REF);
               decl = formals[i].decl;
               tree_set_ref(ref, decl);
               tree_add_attr_int(ref, formal_i, 1);
               break;
            }
         }

         if (decl == NULL)
            sem_error(value, "%s has no formal %s",
                      istr(tree_ident(unit)), istr(tree_ident(ref)));

         if (!sem_static_name(name))
            sem_error(name, "formal name must be static");

         if (conv != NULL) {
            port_mode_t mode = tree_subkind(decl);

            type = tree_type((mode == PORT_INOUT) ? name : conv);

            if (mode == PORT_IN)
               sem_error(name, "output conversion not allowed for formal "
                         "%s with mode IN", istr(tree_ident(decl)));

            if (tree_kind(value) == T_OPEN)
               sem_error(name, "output conversion for formal %s must not "
                         "have OPEN actual", istr(tree_ident(decl)));
         }
         else
            type = tree_type(name);

         break;
      }
   }

   assert(type != NULL);

   if (!sem_check_constrained(value, type))
      return false;

   type_t value_type = tree_type(value);

   if (!type_eq(value_type, type))
      sem_error(value, "type of actual %s does not match type %s of formal "
                "port %s", sem_type_str(value_type),
                sem_type_str(type), istr(tree_ident(decl)));

   if (tree_kind(value) == T_OPEN) {
      port_mode_t mode = tree_subkind(decl);

      if ((mode == PORT_IN) && !tree_has_value(decl))
         sem_error(value, "unconnected port %s with mode IN must have a "
                   "default value", istr(tree_ident(decl)));

      if ((mode != PORT_IN) && type_is_unconstrained(tree_type(decl)))
         sem_error(value, "port %s of unconstrained type %s cannot "
                   "be unconnected", istr(tree_ident(decl)),
                   sem_type_str(type));
   }

   // Check for type conversions and conversion functions
   // These only apply if the class of the formal is not constant

   tree_t actual = NULL;

   if (tree_class(decl) != C_CONSTANT) {
      if (tree_kind(value) == T_TYPE_CONV)
         actual = tree_value(tree_param(value, 0));
      else if (tree_kind(value) == T_FCALL) {
         // Conversion functions are in LRM 93 section 4.3.2.2

         tree_t func = tree_ref(value);
         if ((tree_ports(func) == 1) && (tree_params(value) == 1))
            actual = tree_value(tree_param(value, 0));
      }
   }

   if (actual == NULL)
      actual = value;    // No conversion
   else {
      // LRM 93 section 3.2.1.1 result of a type conversion in an
      // association list cannot be an unconstrained array type
      if (type_is_unconstrained(value_type)
          && type_is_unconstrained(type))
         sem_error(value, "result of conversion for unconstrained formal "
                   "%s must be a constrained array type",
                   istr(tree_ident(decl)));

      if (tree_subkind(decl) == PORT_OUT)
         sem_error(value, "conversion not allowed for formal %s with "
                   "mode OUT", istr(tree_ident(decl)));
   }

   if (!sem_globally_static(actual) && !sem_static_name(actual))
      sem_error(value, "actual must be globally static expression "
                "or locally static name");

   return true;
}

static bool sem_check_map(tree_t t, tree_t unit,
                          tree_formals_t tree_Fs, tree_formal_t tree_F,
                          tree_actuals_t tree_As, tree_actual_t tree_A)
{
   // Check there is an actual for each formal port or generic
   // Rules for maps are described in LRM 93 section 5.2.1.2

   const int nformals = tree_Fs(unit);
   const int nactuals = tree_As(t);

   bool ok = true;

   formal_map_t formals[nformals];

   for (int i = 0; i < nformals; i++) {
      formals[i].decl    = tree_F(unit, i);
      formals[i].have    = false;
      formals[i].partial = false;
   }

   bool has_named = false;

   for (int i = 0; i < nactuals; i++) {
      tree_t p = tree_A(t, i);
      if (tree_subkind(p) != P_NAMED)
         continue;

      if (!has_named) {
         scope_push(NULL);
         top_scope->flags |= SCOPE_FORMAL;

         for (int i = 0; i < nformals; i++)
            (void)scope_insert(formals[i].decl);

         has_named = true;
      }

      ok = sem_check(tree_name(p)) && ok;
   }

   if (has_named)
      scope_pop();

   if (!ok)
      return false;

   for (int i = 0; i < nactuals; i++)
      ok = sem_check_actual(formals, nformals, tree_A(t, i), unit) && ok;

   if (tree_kind(unit) == T_ENTITY) {
      // Component and configuration instantiations must be checked at
      // elaboration time

      for (int i = 0; i < nformals; i++) {
         if (!formals[i].have && !tree_has_value(formals[i].decl)
             && (tree_subkind(formals[i].decl) != PORT_OUT)) {
            error_at(tree_loc(t), "missing actual for formal %s",
                     istr(tree_ident(formals[i].decl)));
            ++errors;
         }
      }
   }

   return ok;
}

static bool sem_find_unit(tree_t t, ident_t name, tree_t *unit)
{
   ident_t lname = ident_until(name, '.');
   lib_t lib = lib_find(istr(lname), false, false);
   if (lib != NULL) {
      if ((*unit = lib_get_check_stale(lib, name)) == NULL)
         sem_error(t, "cannot find unit %s", istr(name));

      return true;
   }
   else
      sem_error(t, "missing library clause for %s", istr(lname));
}

static bool sem_check_instance(tree_t t)
{
   tree_t unit = NULL;
   ident_t name = tree_ident2(t);

   switch (tree_class(t)) {
   case C_ENTITY:
      {
         ident_t prefix = ident_until(name, '-');

         if (!sem_find_unit(t, prefix, &unit))
            return false;

         if (tree_kind(unit) != T_ENTITY)
            sem_error(t, "unit %s is not an entity", istr(prefix));
      }
      break;

   case C_COMPONENT:
      {
         // Find the component declaration
         unit = scope_find(name);
         if (unit == NULL)
            sem_error(t, "no visible declaration for component %s", istr(name));

         if (tree_kind(unit) != T_COMPONENT)
            sem_error(t, "object %s is not a component declaration",
                      istr(name));
      }
      break;

   default:
      sem_error(t, "sorry, this form of instance is not supported yet");
   }

   tree_set_ref(t, unit);

   return sem_check_map(t, unit, tree_ports, tree_port,
                        tree_params, tree_param)
      && sem_check_map(t, unit, tree_generics, tree_generic,
                       tree_genmaps, tree_genmap);
}

static bool sem_check_if(tree_t t)
{
   type_t std_bool = sem_std_type("BOOLEAN");

   tree_t value = tree_value(t);
   if (!sem_check_constrained(value, std_bool))
      return false;

   if (!type_eq(tree_type(value), std_bool))
      sem_error(value, "type of test must be %s but is %s",
                sem_type_str(std_bool),
                sem_type_str(tree_type(value)));

   if (!sem_readable(value))
      return false;

   return sem_check_stmts(t, tree_stmt, tree_stmts(t))
      && sem_check_stmts(t, tree_else_stmt, tree_else_stmts(t));
}

static bool sem_subtype_locally_static(type_t type)
{
   // Rules for locally static subtypes are in LRM 93 7.4.1

   if (type_is_unconstrained(type))
      return false;

   switch (type_kind(type)) {
   case T_CARRAY:
   case T_SUBTYPE:
      {
         const int ndims = type_dims(type);
         for (int i = 0; i < ndims; i++) {
            range_t r = type_dim(type, i);
            if (!sem_locally_static(r.left)
                || !sem_locally_static(r.right))
               return false;
         }

         return true;
      }
   default:
      return true;
   }
}

static bool sem_locally_static(tree_t t)
{
   // Rules for locally static expressions are in LRM 93 7.4.1

   type_t type = tree_type(t);
   tree_kind_t kind = tree_kind(t);

   // Any literal other than of type time
   if (kind == T_LITERAL) {
      type_t std_time = sem_std_type("TIME");
      return !type_eq(type, std_time);
   }
   else if ((kind == T_REF) && (tree_kind(tree_ref(t)) == T_ENUM_LIT))
      return true;
   else if (kind == T_OPEN)
      return true;

   if (kind == T_REF) {
      tree_t decl = tree_ref(t);
      const tree_kind_t dkind = tree_kind(decl);

      // A constant reference with a locally static value
      if (dkind == T_CONST_DECL) {
         tree_t value = tree_value(decl);
         return sem_subtype_locally_static(tree_type(decl))
            && sem_locally_static(value)
            && !tree_attr_int(value, unconstrained_i, 0);
      }
      else if ((standard() >= STD_08 || relax & RELAX_GENERIC_STATIC)
               && dkind == T_PORT_DECL) {
         // [2008] A generic reference with a locally static subtype
         return tree_class(decl) == C_CONSTANT
            && sem_subtype_locally_static(tree_type(decl));
      }
   }

   // An alias of a locally static name
   if (kind == T_ALIAS)
      return sem_locally_static(tree_value(t));

   // A function call of an implicit operator with locally static actuals
   if (kind == T_FCALL) {
      tree_t decl = tree_ref(t);

      ident_t builtin = tree_attr_str(decl, builtin_i);
      if (builtin == NULL)
         return false;

      bool all_static = true;
      const int nparams = tree_params(t);
      for (int i = 0; i < nparams; i++) {
         tree_t p = tree_param(t, i);
         all_static = all_static && sem_locally_static(tree_value(p));
      }
      return all_static;
   }

   if (kind == T_ATTR_REF) {
      // A predefined attribute other than 'PATH_NAME whose prefix has a
      // locally static subtype
      tree_t decl = tree_ref(t);
      if (icmp(tree_ident(t), "PATH_NAME"))
         return false;
      else if (tree_kind(decl) == T_FUNC_DECL) {
         assert(tree_attr_str(decl, builtin_i));

         type_t type = tree_type(tree_ref(tree_name(t)));
         return sem_subtype_locally_static(type);
      }

      // A user-defined attribute whose value is a locally static expression
      assert(tree_has_value(t));
      return sem_locally_static(tree_value(t));
   }

   // A qualified expression whose operand is locally static
   if (kind == T_QUALIFIED)
      return sem_locally_static(tree_value(t));

   // A type conversion whose expression is locally static
   if (kind == T_TYPE_CONV)
      return sem_locally_static(tree_value(tree_param(t, 0)));

   // Aggregates must have locally static range and all elements
   // must have locally static values
   if (kind == T_AGGREGATE) {
      if (tree_attr_int(t, unconstrained_i, 0))
         return false;

      if (type_is_array(type)) {
         range_t r = type_dim(type, 0);
         if (r.kind != RANGE_TO && r.kind != RANGE_DOWNTO)
            return false;

         if (!sem_locally_static(r.left) || !sem_locally_static(r.right))
            return false;
      }

      const int nassocs = tree_assocs(t);
      for (int i = 0; i < nassocs; i++) {
         tree_t a = tree_assoc(t, i);
         if ((tree_subkind(a) == A_NAMED) && !sem_locally_static(tree_name(a)))
            return false;

         if (!sem_locally_static(tree_value(a)))
            return false;
      }

      return true;
   }

   // A record field name
   if ((kind == T_REF) && (tree_kind(tree_ref(t)) == T_FIELD_DECL))
      return true;

   return false;
}

static bool sem_static_name(tree_t t)
{
   // Rules for static names are in LRM 93 6.1

   switch (tree_kind(t)) {
   case T_REF:
      {
         tree_t decl = tree_ref(t);
         switch (tree_kind(decl)) {
         case T_SIGNAL_DECL:
         case T_VAR_DECL:
         case T_CONST_DECL:
         case T_PORT_DECL:
         case T_TYPE_DECL:
         case T_ENTITY:
         case T_ARCH:
         case T_PACK_BODY:
         case T_PACKAGE:
         case T_FUNC_BODY:
         case T_PROC_BODY:
         case T_FUNC_DECL:
         case T_PROC_DECL:
         case T_PROCESS:
         case T_BLOCK:
            return true;
         case T_ALIAS:
            return sem_static_name(tree_value(decl));
         default:
            return false;
         }
      }

   case T_RECORD_REF:
   case T_ALL:
      return sem_static_name(tree_value(t));

   case T_ARRAY_REF:
      {
         if (!sem_static_name(tree_value(t)))
            return false;

         const int nparams = tree_params(t);
         for (int i = 0; i < nparams; i++) {
            if (!sem_globally_static(tree_value(tree_param(t, i))))
               return false;
         }

         return true;
      }

   case T_ARRAY_SLICE:
      {
         if (!sem_static_name(tree_value(t)))
            return false;

         range_t r = tree_range(t);

         return (r.kind == RANGE_TO || r.kind == RANGE_DOWNTO)
            && sem_globally_static(r.left)
            && sem_globally_static(r.right);
      }
      // Fall-through

   default:
      return false;
   }
}

static bool sem_globally_static(tree_t t)
{
   // Rules for globally static expressions are in LRM 93 7.4.2

   type_t type = tree_type(t);
   tree_kind_t kind = tree_kind(t);

   // A literal of type TIME

   if ((kind == T_REF) && (tree_kind(tree_ref(t)) == T_UNIT_DECL)) {
      type_t std_time = sem_std_type("TIME");
      if (type_eq(type, std_time))
         return true;
   }

   // A locally static primary

   if (sem_locally_static(t))
      return true;

   // A generic constant, generate parameter, or constant

   if (kind == T_REF) {
      tree_t decl = tree_ref(t);
      tree_kind_t decl_kind = tree_kind(decl);
      if ((decl_kind == T_PORT_DECL) && (tree_class(decl) == C_CONSTANT))
         return true;
      else if (decl_kind == T_GENVAR)
         return true;
      else if (decl_kind == T_CONST_DECL)
         return true;
   }

   // An alias whose aliased name is globally static

   if (kind == T_ALIAS)
      return sem_globally_static(tree_value(t));

   // Aggregates must have globally static range and all elements
   // must have globally static values
   if (kind == T_AGGREGATE) {
      range_t r = type_dim(type, 0);
      if (r.kind != RANGE_TO && r.kind != RANGE_DOWNTO)
         return false;

      if (!sem_globally_static(r.left) || !sem_globally_static(r.right))
         return false;

      const int nassocs = tree_assocs(t);
      for (int i = 0; i < nassocs; i++) {
         tree_t a = tree_assoc(t, i);
         if ((tree_subkind(a) == A_NAMED) && !sem_globally_static(tree_name(a)))
            return false;

         if (!sem_globally_static(tree_value(a)))
            return false;
      }

      return true;
   }

   // TODO: clause h

   // A function call of a pure function with globally static actuals
   if ((kind == T_FCALL) || (kind == T_CONCAT)) {
      if (kind == T_FCALL) {
         tree_t decl = tree_ref(t);
         if (tree_attr_int(decl, ident_new("impure"), 0))
            return false;
      }

      bool all_static = true;
      const int nparams = tree_params(t);
      for (int i = 0; i < nparams; i++) {
         tree_t p = tree_param(t, i);
         all_static = all_static && sem_globally_static(tree_value(p));
      }
      return all_static;
   }

   if (kind == T_ATTR_REF) {
      // A predefined attribute prefix has a globally static subtype
      tree_t decl = tree_ref(t);
      ident_t attr = tree_ident(t);
      if (icmp(attr, "EVENT") || icmp(attr, "ACTIVE")
          || icmp(attr, "LAST_EVENT") || icmp(attr, "LAST_ACTIVE")
          || icmp(attr, "LAST_VALUE") || icmp(attr, "DRIVING")
          || icmp(attr, "DRIVING_VALUE"))
         return false;   // Clause k
      else if (tree_kind(decl) == T_FUNC_DECL) {
         assert(tree_attr_str(decl, builtin_i));

         // Check for globally static subtype
         type_t type = tree_type(tree_ref(tree_name(t)));

         switch (type_kind(type)) {
         case T_CARRAY:
         case T_SUBTYPE:
            {
               const int ndims = type_dims(type);
               for (int i = 0; i < ndims; i++) {
                  range_t r = type_dim(type, i);
                  if (!sem_globally_static(r.left)
                      || !sem_globally_static(r.right))
                     return false;
               }

               return true;
            }
         default:
            return true;
         }
      }

      // A user-defined attribute whose value is a globally static expression
      assert(tree_has_value(t));
      return sem_globally_static(tree_value(t));
   }

   // A qualified expression whose operand is globally static

   if (kind == T_QUALIFIED)
      return sem_globally_static(tree_value(t));

   // A type conversion whose operand is globally static

   if (kind == T_TYPE_CONV)
      return sem_globally_static(tree_value(tree_param(t, 0)));

   // TODO: clauses o, p

   // A sub-element or slice where indexes are globally static

   if (kind == T_ARRAY_REF) {
      if (!sem_globally_static(tree_value(t)))
         return false;

      const int nparams = tree_params(t);
      for (int i = 0; i < nparams; i++) {
         if (!sem_globally_static(tree_value(tree_param(t, i))))
            return false;
      }

      return true;
   }
   else if (kind == T_ARRAY_SLICE) {
      if (!sem_globally_static(tree_value(t)))
         return false;

      range_t r = tree_range(t);
      if (!sem_globally_static(r.left) || !sem_globally_static(r.right))
         return false;

      return true;
   }
   else if (kind == T_RECORD_REF)
      return sem_globally_static(tree_value(t));

   return false;
}

static bool sem_check_case(tree_t t)
{
   tree_t test = tree_value(t);
   if (!sem_check(test))
      return false;

   type_t type = tree_type(test);

   bool ok = true;
   const int nassocs = tree_assocs(t);
   for (int i = 0; i < nassocs; i++) {
      tree_t a = tree_assoc(t, i);
      switch (tree_subkind(a)) {
      case A_OTHERS:
         if (i != tree_assocs(t) - 1)
            sem_error(t, "others choice must appear last");
         break;

      case A_NAMED:
         {
            tree_t name = tree_name(a);
            if ((ok = sem_check_constrained(name, type) && ok)) {
               if (!type_eq(tree_type(name), type))
                  sem_error(name, "case choice must have type %s",
                            sem_type_str(type));
               else if (!sem_locally_static(name))
                  sem_error(name, "case choice must be locally static");

               tree_set_type(name, type);
            }
         }
         break;

      case A_RANGE:
         {
            range_t r = tree_range(a);
            if ((ok = sem_check_range(&r, type) && ok)) {
               if (!type_eq(tree_type(r.left), type))
                  sem_error(r.left, "case choice range must have type %s",
                            sem_type_str(type));
               else if (!sem_locally_static(r.left))
                  sem_error(r.left, "left index of case choice range is "
                            "not locally static");
               else if (!sem_locally_static(r.right))
                  sem_error(r.right, "right index of case choice range is "
                            "not locally static");
            }
         }
         break;

      default:
         sem_error(a, "sorry, this form of choice is not supported");
      }

      ok = sem_check(tree_value(a)) && ok;
   }

   return ok;
}

static bool sem_check_return(tree_t t)
{
   if (top_scope->subprog == NULL)
      sem_error(t, "return statement not allowed outside subprogram");

   if (tree_has_value(t)) {
      if (tree_kind(top_scope->subprog) == T_PROC_BODY)
         sem_error(t, "cannot return a value from a procedure");

      type_t expect = type_result(tree_type(top_scope->subprog));

      if (!sem_check_constrained(tree_value(t), expect))
         return false;

      if (!type_eq(tree_type(tree_value(t)), expect))
         sem_error(t, "expected return type %s", sem_type_str(expect));
   }

   return true;
}

static bool sem_check_while(tree_t t)
{
   type_t std_bool = sem_std_type("BOOLEAN");

   tree_t value = tree_value(t);
   if (!sem_check_constrained(value, std_bool))
      return false;

   if (!type_eq(tree_type(value), std_bool))
      sem_error(value, "type of loop condition must be %s but is %s",
                sem_type_str(std_bool),
                sem_type_str(tree_type(value)));

   loop_push(tree_ident(t));

   const bool ok = sem_check_stmts(t, tree_stmt, tree_stmts(t));

   loop_pop();

   return ok;
}

static bool sem_check_for(tree_t t)
{
   range_t r = tree_range(t);
   const bool is_range_expr = (r.kind == RANGE_EXPR);
   if (!sem_check_range(&r, NULL))
      return false;
   tree_set_range(t, r);

   tree_t idecl = tree_new(T_VAR_DECL);
   tree_set_ident(idecl, tree_ident2(t));
   tree_set_loc(idecl, tree_loc(t));
   tree_set_type(idecl, tree_type(r.left));

   if (is_range_expr) {
      // Find the variable X in X'RANGE
      tree_t range_var = tree_ref(tree_name(r.left));
      tree_add_attr_tree(idecl, ident_new("range_var"), range_var);
   }

   tree_add_decl(t, idecl);

   scope_push(NULL);
   scope_insert(idecl);
   loop_push(tree_ident(t));

   const bool ok = sem_check_stmts(t, tree_stmt, tree_stmts(t));

   loop_pop();
   scope_pop();
   return ok;
}

static bool sem_check_block(tree_t t)
{
   scope_push(NULL);

   sem_add_attributes(t, false);

   bool ok = true;

   const int ndecls = tree_decls(t);
   for (int i = 0; i < ndecls; i++)
      ok = sem_check(tree_decl(t, i)) && ok;

   ok = ok && sem_check_stmts(t, tree_stmt, tree_stmts(t));

   ok = scope_run_deferred_checks() && ok;

   scope_pop();
   return ok;
}

static bool sem_check_loop_control(tree_t t)
{
   if (loop_stack == NULL)
      sem_error(t, "cannot use %s outside loop",
                (tree_kind(t) == T_EXIT) ? "exit" : "next");

   if (tree_has_ident2(t)) {
      ident_t label = tree_ident2(t);
      loop_stack_t *it;
      for (it = loop_stack; (it != NULL) && (it->name != label); it = it->up)
         ;

      if (it == NULL)
         sem_error(t, "no nested loop with label %s", istr(label));
   }
   else
      tree_set_ident2(t, loop_stack->name);

   if (tree_has_value(t)) {
      tree_t value = tree_value(t);
      if (!sem_check(value))
         return false;

      type_t std_bool = sem_std_type("BOOLEAN");
      if (!type_eq(tree_type(value), std_bool))
         sem_error(value, "type of %s condition must be %s but is %s",
                   (tree_kind(t) == T_EXIT) ? "exit" : "next",
                   sem_type_str(std_bool),
                   sem_type_str(tree_type(value)));
   }

   return true;
}

static bool sem_check_select(tree_t t)
{
   if (!sem_check(tree_value(t)))
      return false;

   type_t value_type = tree_type(tree_value(t));

   const int nassocs = tree_assocs(t);
   for (int i = 0; i < nassocs; i++) {
      tree_t a = tree_assoc(t, i);
      if (tree_subkind(a) == A_NAMED) {
         tree_t name = tree_name(a);
         if (!sem_check_constrained(name, value_type))
            return false;
         else if (!type_eq(tree_type(name), value_type))
            sem_error(name, "choice must have type %s",
                      sem_type_str(value_type));
         else if (!sem_locally_static(name))
            sem_error(name, "choice must be locally static");
      }

      if (!sem_check(tree_value(a)))
         return false;
   }

   return true;
}

static bool sem_check_attr_decl(tree_t t)
{
   type_t type = tree_type(t);
   if (!sem_check_type(t, &type))
      return false;

   tree_set_type(t, type);

   scope_apply_prefix(t);
   return scope_insert(t);
}

static bool sem_check_attr_spec(tree_t t)
{
   tree_t attr_decl = scope_find(tree_ident(t));
   if (attr_decl == NULL)
      sem_error(t, "undefined attribute %s", istr(tree_ident(t)));

   if (tree_kind(attr_decl) != T_ATTR_DECL)
      sem_error(t, "name %s is not an attribute declaration",
                istr(tree_ident(t)));

   type_t type = tree_type(attr_decl);

   tree_t value = tree_value(t);
   if (!sem_check_constrained(value, type))
      return false;

   if (!type_eq(type, tree_type(value)))
      sem_error(t, "expected attribute type %s", sem_type_str(type));

   // Attributes of labels are ignored currently
   class_t class = tree_class(t);
   if (class == C_LABEL)
      return true;

   tree_t obj_decl = scope_find(tree_ident2(t));
   if (obj_decl == NULL)
      sem_error(t, "no visible declaration for %s", istr(tree_ident2(t)));

   class_t obj_class = class_of(obj_decl);
   if (obj_class != class)
      sem_error(t, "class of object %s is %s not %s", istr(tree_ident2(t)),
                class_str(obj_class), class_str(class));

   tree_add_attr_tree(obj_decl, tree_ident(t), value);

   return true;
}

static bool sem_check_if_generate(tree_t t)
{
   type_t std_bool = sem_std_type("BOOLEAN");
   tree_t value = tree_value(t);

   if (!sem_check_constrained(value, std_bool))
      return false;

   if (!type_eq(tree_type(value), std_bool))
      sem_error(value, "condition of generate statement must be BOOLEAN");

   if (!sem_globally_static(value))
      sem_error(value, "condition of generate statement must be static");

   scope_push(NULL);

   bool ok = true;

   const int ndecls = tree_decls(t);
   for (int i = 0; i < ndecls; i++)
      ok = sem_check(tree_decl(t, i)) && ok;

   ok = ok && sem_check_stmts(t, tree_stmt, tree_stmts(t));

   ok = scope_run_deferred_checks() && ok;

   scope_pop();
   return ok;
}

static bool sem_check_for_generate(tree_t t)
{
   range_t r = tree_range(t);
   if (!sem_check_range(&r, NULL))
      return false;
   tree_set_range(t, r);

   if (!sem_globally_static(r.left))
      sem_error(r.left, "range of generate statement must be static");
   else if (!sem_globally_static(r.right))
      sem_error(r.right, "range of generate statement must be static");

   tree_t idecl = tree_new(T_GENVAR);
   tree_set_ident(idecl, tree_ident2(t));
   tree_set_loc(idecl, tree_loc(t));
   tree_set_type(idecl, tree_type(r.left));

   tree_set_ref(t, idecl);

   scope_push(NULL);
   scope_insert(idecl);

   bool ok = true;

   const int ndecls = tree_decls(t);
   for (int i = 0; i < ndecls; i++)
      ok = sem_check(tree_decl(t, i)) && ok;

   ok = ok && sem_check_stmts(t, tree_stmt, tree_stmts(t));

   ok = scope_run_deferred_checks() && ok;

   tree_add_decl(t, idecl);

   scope_pop();
   return ok;
}

static bool sem_check_open(tree_t t)
{
   type_t type;
   if (type_set_any(&type)) {
      tree_set_type(t, type);
      return true;
   }
   else
      sem_error(t, "OPEN cannot be used here");
}

static bool sem_check_file_decl(tree_t t)
{
   // Rules for file declarations are in LRM 93 section 4.3.1.4

   type_t type = tree_type(t);
   if (!sem_check_type(t, &type))
      return false;

   tree_set_type(t, type);

   if (type_kind(type) != T_FILE)
      sem_error(t, "file declarations must have file type");

   if (tree_has_value(t)) {
      type_t string = sem_std_type("STRING");
      tree_t value = tree_value(t);
      if (!sem_check_constrained(value, string))
         return false;

      if (!type_eq(tree_type(value), string))
         sem_error(value, "file name must have type STRING");

      type_t open_kind = sem_std_type("FILE_OPEN_KIND");
      tree_t mode = tree_file_mode(t);
      if (!sem_check_constrained(mode, open_kind))
         return false;

      if (!type_eq(tree_type(mode), open_kind))
         sem_error(mode, "open mode must have type FILE_OPEN_KIND");
   }

   scope_apply_prefix(t);
   return scope_insert(t);
}

static bool sem_is_access(type_t t)
{
   return type_kind(t) == T_ACCESS;
}

static bool sem_check_new(tree_t t)
{
   // Rules for allocators are in LRM 93 section 7.3.6

   tree_t value = tree_value(t);

   if (!type_set_restrict(sem_is_access))
      sem_error(t, "no access type in context");

   type_t access_type;
   if (!type_set_uniq(&access_type))
      sem_error(t, "context does not contain unique access type");

   type_t type = NULL;

   switch (tree_kind(value)) {
   case T_ARRAY_SLICE:
      {
         range_t r = tree_range(value);
         if (!sem_check_range(&r, sem_std_type("INTEGER")))
            return false;
         tree_set_range(value, r);

         tree_t ref = tree_value(value);
         if (tree_kind(ref) != T_REF)
            sem_error(t, "invalid array allocator expression");

         type_t base = type_new(T_UNRESOLVED);
         type_set_ident(base, tree_ident(ref));
         if (!sem_check_type(value, &base))
            return false;

         type = type_new(T_SUBTYPE);
         type_set_base(type, base);
         type_add_dim(type, r);

         tree_set_value(t, make_default_value(type, tree_loc(value)));
      }
      break;

   case T_REF:
      {
         type = type_new(T_UNRESOLVED);
         type_set_ident(type, tree_ident(value));
         if (!sem_check_type(value, &type))
            return false;

         tree_set_value(t, make_default_value(type, tree_loc(t)));
      }
      break;

   case T_QUALIFIED:
      if (!sem_check_qualified(value))
         return false;
      type = tree_type(value);
      break;

   default:
      sem_error(t, "invalid allocator expression");
   }

   if (!type_eq(type, type_access(access_type)))
      sem_error(value, "type of allocator expresion %s does not match "
                "access type %s", sem_type_str(type),
                sem_type_str(type_access(access_type)));

   tree_set_type(t, access_type);
   return true;
}

static bool sem_check_all(tree_t t)
{
   tree_t value = tree_value(t);
   if (!sem_check_constrained(value, NULL))
      return false;

   type_t value_type = tree_type(value);

   if (type_kind(value_type) != T_ACCESS)
      sem_error(value, "expression type %s is not access",
                sem_type_str(value_type));

   tree_set_type(t, type_access(value_type));
   return true;
}

static bool sem_bind(tree_t spec, tree_t inst)
{
   ident_t cname = tree_ident2(spec);

   tree_t comp = scope_find(cname);
   if (comp == NULL)
      sem_error(spec, "component %s not found", istr(cname));
   else if (tree_kind(comp) != T_COMPONENT)
      sem_error(spec, "object %s is not a component declaration", istr(cname));

   if (tree_class(inst) != C_COMPONENT)
      sem_error(spec, "specification may only be used with component "
                "instances");

   if (!tree_has_ref(inst))
      return false;

   if (tree_ref(inst) != comp)
      sem_error(spec, "component mismatch for instance %s: expected %s but "
                "specification has %s", istr(tree_ident(spec)),
                istr(tree_ident(tree_ref(inst))), istr(cname));

   if (tree_has_spec(inst)) {
      tree_t exist = tree_spec(inst);
      if (tree_has_ident(exist))  // Not an OTHERS specification
         sem_error(spec, "instance %s is already bound by a specification",
                   istr(tree_ident(inst)));
   }

   tree_set_spec(inst, spec);
   return true;
}

static bool sem_check_spec(tree_t t)
{
   if (tree_has_value(t)) {
      if (!sem_check(tree_value(t)))
         return false;
   }

   ident_t iname = tree_has_ident(t) ? tree_ident(t) : NULL;

   enum { NAMED, ALL, OTHERS } kind;

   if (iname == NULL)
      kind = OTHERS;
   else if (iname == all_i)
      kind = ALL;
   else
      kind = NAMED;

   if (kind == NAMED) {
      tree_t inst = scope_find(iname);
      if (inst == NULL)
         sem_error(t, "instance %s not found", istr(iname));
      else if (tree_kind(inst) != T_INSTANCE)
         sem_error(t, "object %s is not an instance", istr(iname));

      return sem_bind(t, inst);
   }
   else {
      ident_t cname = tree_ident2(t);
      hash_iter_t it = HASH_BEGIN;
      tree_t obj;
      while (scope_walk(&it, &obj)) {
         if (tree_kind(obj) != T_INSTANCE)
            continue;

         if (tree_class(obj) != C_COMPONENT)
            continue;

         if (tree_ident2(obj) == cname) {
            if ((kind == ALL) || !tree_has_spec(obj)) {
               if (!sem_bind(t, obj))
                  return false;
            }
         }
      }

      return true;
   }
}

static bool sem_check_binding(tree_t t)
{
   if (tree_params(t) > 0)
      sem_error(t, "sorry, bindings with port maps are not yet supported");

   if (tree_genmaps(t) > 0)
      sem_error(t, "sorry, bindings with generic maps are not yet supported");

   tree_t unit;
   if (!sem_find_unit(t, tree_ident(t), &unit))
      return false;

   switch (tree_class(t)) {
   case C_ENTITY:
      if (tree_kind(unit) != T_ENTITY)
         sem_error(t, "unit %s is not an entity", istr(tree_ident(t)));
      break;

   case C_CONFIGURATION:
      if (tree_kind(unit) != T_CONFIG)
         sem_error(t, "unit %s is not a configuration", istr(tree_ident(t)));
      break;

   default:
      assert(false);
   }

   return true;
}

static bool sem_copy_instances(tree_t t, void *context)
{
   switch (tree_kind(t)) {
   case T_INSTANCE:
   case T_ARCH:
      return true;
   default:
      return false;
   }
}

static bool sem_check_configuration(tree_t t)
{
   lib_t work = lib_work();

   ident_t name_qual = ident_prefix(lib_name(work), tree_ident2(t), '.');
   tree_t arch = lib_get(work, name_qual);
   if (arch == NULL)
      sem_error(t, "architecture %s of entity %s not found in library %s",
                istr(ident_rfrom(name_qual, '-')),
                istr(ident_until(name_qual, '-')),
                istr(lib_name(work)));

   tree_t copy = tree_copy(arch, sem_copy_instances, NULL);
   tree_set_ident(copy, ident_prefix(name_qual, tree_ident(t), '-'));
   lib_put(lib_work(), copy);

   scope_push(NULL);

   const int nadecls = tree_decls(copy);
   for (int i = 0; i < nadecls; i++)
      scope_insert(tree_decl(copy, i));

   const int nastmts = tree_stmts(copy);
   for (int i = 0; i < nastmts; i++)
      scope_insert(tree_stmt(copy, i));

   scope_push(NULL);

   bool ok = true;
   const int ndecls = tree_decls(t);
   for (int i = 0; i < ndecls; i++)
      ok = sem_check(tree_decl(t, i));

   ok = scope_run_deferred_checks() && ok;

   scope_pop();
   scope_pop();

   return ok;
}

static bool sem_check_prot_body(tree_t t)
{
   // Rules for protected type bodies are in LRM 00 section 3.5.2

   ident_t name = tree_ident(t);

   tree_t tdecl = scope_find(name);
   if (tdecl == NULL)
      sem_error(t, "no protected type declaration for %s found", istr(name));
   else if (tree_kind(tdecl) != T_TYPE_DECL)
      sem_error(t, "object %s is not a protected type declaration", istr(name));

   type_t type = tree_type(tdecl);
   if (type_kind(type) != T_PROTECTED)
      sem_error(t, "object %s is not a protected type declaration", istr(name));

   if (type_has_body(type))
      sem_error(t, "protected type %s already has body", istr(name));

   type_set_body(type, t);
   tree_set_type(t, type);

   scope_push(ident_prefix(top_scope->prefix, name, '.'));
   top_scope->flags |= SCOPE_PROTECTED;

   const int ntdecls = type_decls(type);
   for (int i = 0; i < ntdecls; i++) {
      tree_t d = type_decl(type, i);

      (void)sem_declare(d, true);

      ident_t unqual = ident_rfrom(tree_ident(d), '.');
      if (unqual != NULL)
         scope_insert_alias(d, unqual);
   }

   bool ok = true;
   const int ndecls = tree_decls(t);
   for (int i = 0; i < ndecls; i++) {
      tree_t d = tree_decl(t, i);
      ident_t unqual = tree_ident(d);

      ok = sem_check(d) && ok;
      scope_insert_alias(d, unqual);
   }

   scope_pop();
   return ok;
}

static void sem_intern_strings(void)
{
   // Intern some commonly used strings

   builtin_i        = ident_new("builtin");
   std_standard_i   = ident_new("STD.STANDARD");
   formal_i         = ident_new("formal");
   locally_static_i = ident_new("locally_static");
   elab_copy_i      = ident_new("elab_copy");
   all_i            = ident_new("all");
   shared_i         = ident_new("shared");
   unconstrained_i  = ident_new("unconstrained");
   protected_i      = ident_new("protected");
   impure_i         = ident_new("impure");
}

bool sem_check(tree_t t)
{
   static bool have_interned = false;
   if (!have_interned) {
      sem_intern_strings();
      relax = opt_get_int("relax");
      have_interned = true;
   }

   switch (tree_kind(t)) {
   case T_ARCH:
      return sem_check_arch(t);
   case T_PACKAGE:
      return sem_check_package(t);
   case T_ENTITY:
      return sem_check_entity(t);
   case T_TYPE_DECL:
      return sem_check_type_decl(t);
   case T_PORT_DECL:
      return sem_check_port_decl(t);
   case T_SIGNAL_DECL:
   case T_VAR_DECL:
   case T_CONST_DECL:
      return sem_check_decl(t);
   case T_PROCESS:
      return sem_check_process(t);
   case T_VAR_ASSIGN:
      return sem_check_var_assign(t);
   case T_SIGNAL_ASSIGN:
      return sem_check_signal_assign(t);
   case T_FCALL:
      return sem_check_fcall(t);
   case T_LITERAL:
      return sem_check_literal(t);
   case T_REF:
      return sem_check_ref(t);
   case T_WAIT:
      return sem_check_wait(t);
   case T_ASSERT:
   case T_CASSERT:
      return sem_check_assert(t);
   case T_QUALIFIED:
      return sem_check_qualified(t);
   case T_FUNC_DECL:
      return sem_check_func_decl(t);
   case T_AGGREGATE:
      return sem_check_aggregate(t);
   case T_ATTR_REF:
      return sem_check_attr_ref(t);
   case T_ARRAY_REF:
      return sem_check_array_ref(t);
   case T_ARRAY_SLICE:
      return sem_check_array_slice(t);
   case T_INSTANCE:
      return sem_check_instance(t);
   case T_IF:
      return sem_check_if(t);
   case T_NULL:
      return true;
   case T_PACK_BODY:
      return sem_check_package_body(t);
   case T_FUNC_BODY:
      return sem_check_func_body(t);
   case T_RETURN:
      return sem_check_return(t);
   case T_CASSIGN:
      return sem_check_cassign(t);
   case T_WHILE:
      return sem_check_while(t);
   case T_ALIAS:
      return sem_check_alias(t);
   case T_FOR:
      return sem_check_for(t);
   case T_PROC_DECL:
      return sem_check_proc_decl(t);
   case T_PROC_BODY:
      return sem_check_proc_body(t);
   case T_BLOCK:
      return sem_check_block(t);
   case T_CASE:
      return sem_check_case(t);
   case T_EXIT:
   case T_NEXT:
      return sem_check_loop_control(t);
   case T_CONCAT:
      return sem_check_concat(t);
   case T_PCALL:
   case T_CPCALL:
      return sem_check_pcall(t);
   case T_SELECT:
      return sem_check_select(t);
   case T_ATTR_SPEC:
      return sem_check_attr_spec(t);
   case T_ATTR_DECL:
      return sem_check_attr_decl(t);
   case T_COMPONENT:
      return sem_check_component(t);
   case T_IF_GENERATE:
      return sem_check_if_generate(t);
   case T_FOR_GENERATE:
      return sem_check_for_generate(t);
   case T_OPEN:
      return sem_check_open(t);
   case T_FIELD_DECL:
      return sem_check_field_decl(t);
   case T_FILE_DECL:
      return sem_check_file_decl(t);
   case T_NEW:
      return sem_check_new(t);
   case T_ALL:
      return sem_check_all(t);
   case T_RECORD_REF:
      return sem_check_record_ref(t);
   case T_UNIT_DECL:
      return sem_check_unit_decl(t);
   case T_USE:
      return sem_check_use_clause(t);
   case T_TYPE_CONV:
      return sem_check_conversion(t);
   case T_SPEC:
      scope_defer_check(sem_check_spec, t);
      return true;
   case T_BINDING:
      return sem_check_binding(t);
   case T_LIBRARY:
      return sem_check_library_clause(t);
   case T_CONFIG:
      return sem_check_configuration(t);
   case T_PROT_BODY:
      return sem_check_prot_body(t);
   default:
      sem_error(t, "cannot check %s", tree_kind_str(tree_kind(t)));
   }
}

int sem_errors(void)
{
   return errors;
}

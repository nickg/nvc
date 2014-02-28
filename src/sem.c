//
//  Copyright (C) 2011-2014  Nick Gasson
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

struct scope {
   struct scope *down;

   hash_t       *decls;
   tree_t        subprog;

   // For design unit scopes
   ident_t       prefix;
   ident_list_t *imported;
   bool          is_package;
};

struct loop_stack {
   struct loop_stack *up;
   ident_t            name;
};

#define MAX_OVERLOADS 128

struct type_set {
   type_t   *members;
   unsigned  n_members;
   unsigned  alloc;

   struct type_set *down;
};

typedef struct {
   const loc_t *loc;
   lib_t        lib;
   bool         error;
} lib_walk_params_t;

typedef tree_t (*get_fn_t)(tree_t);
typedef void (*set_fn_t)(tree_t, tree_t);

static bool sem_check_constrained(tree_t t, type_t type);
static bool sem_check_array_ref(tree_t t);
static bool sem_declare(tree_t decl);
static bool sem_locally_static(tree_t t);
static bool sem_globally_static(tree_t t);
static tree_t sem_check_lvalue(tree_t t);
static bool sem_check_type(tree_t t, type_t *ptype);
static bool sem_static_name(tree_t t);
static bool sem_check_range(range_t *r, type_t context);
static type_t sem_index_type(type_t type, int dim);
static unsigned sem_array_dimension(type_t a);

static struct scope      *top_scope = NULL;
static int                errors = 0;
static struct type_set   *top_type_set = NULL;
static struct loop_stack *loop_stack = NULL;
static ident_t            builtin_i;
static ident_t            std_standard_i;
static ident_t            formal_i;
static ident_t            locally_static_i;
static ident_t            elab_copy_i;

#define sem_error(t, ...) do {                        \
      error_at(t ? tree_loc(t) : NULL , __VA_ARGS__); \
      errors++;                                       \
      return false;                                   \
   } while (0)

static void scope_push(ident_t prefix)
{
   struct scope *s = xmalloc(sizeof(struct scope));
   s->decls      = hash_new(1024, false);
   s->prefix     = prefix;
   s->imported   = NULL;
   s->down       = top_scope;
   s->subprog    = (top_scope ? top_scope->subprog : NULL) ;
   s->is_package = false;

   top_scope = s;
}

static void scope_pop(void)
{
   assert(top_scope != NULL);

   ident_list_free(top_scope->imported);
   hash_free(top_scope->decls);

   struct scope *s = top_scope;
   top_scope = s->down;
   free(s);
}

static void scope_apply_prefix(tree_t t)
{
   if (top_scope->prefix)
      tree_set_ident(t, ident_prefix(top_scope->prefix,
                                     tree_ident(t), '.'));
}

static tree_t scope_find_in(ident_t i, struct scope *s, bool recur, int k)
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

         const bool builtin = (tree_attr_str(existing, builtin_i) != NULL);
         if (builtin && type_eq(tree_type(t), tree_type(existing))) {
            // Allow builtin functions to be hidden
            hash_replace(top_scope->decls, existing, t);
            return true;
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
   struct loop_stack *ls = xmalloc(sizeof(struct loop_stack));
   ls->up   = loop_stack;
   ls->name = name;

   loop_stack = ls;
}

static void loop_pop(void)
{
   struct loop_stack *tmp = loop_stack->up;
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
   for (struct scope *s = top_scope; s != NULL; s = s->down) {
      for (struct ident_list *i = s->imported; i != NULL; i = i->next) {
         ident_t search = ident_prefix(i->ident, suffix_i, '.');
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
   for (struct scope *s = top_scope; s != NULL; s = s->down) {
      struct ident_list *it;
      for (it = s->imported; it != NULL; it = it->next) {
         if (it->ident == unit_name)
            return true;
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
      if ((kind == T_ATTR_SPEC) || (kind == T_CONTEXT))
         continue;

      if (!sem_declare(decl))
         return false;

      // Make unqualified and package qualified names visible
      const char *tmp = istr(tree_ident(decl));
      const char *pqual = strchr(tmp, '.');
      if (pqual != NULL)
         scope_insert_alias(decl, ident_new(pqual + 1));
      if (all) {
         const char *unqual = strrchr(tmp, '.');
         if (unqual != NULL)
            scope_insert_alias(decl, ident_new(unqual + 1));
      }
   }

   ident_list_add(&top_scope->imported, unit_name);
   return true;
}

static void type_set_push(void)
{
   struct type_set *t = xmalloc(sizeof(struct type_set));
   t->n_members = 0;
   t->alloc     = 32;
   t->members   = xmalloc(t->alloc * sizeof(type_t));
   t->down      = top_type_set;

   top_type_set = t;
}

static void type_set_pop(void)
{
   assert(top_type_set != NULL);

   struct type_set *old = top_type_set;
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

   if (top_type_set->n_members == top_type_set->alloc) {
      top_type_set->alloc *= 2;
      top_type_set->members =
         xrealloc(top_type_set->members, top_type_set->alloc * sizeof(type_t));
   }

   top_type_set->members[top_type_set->n_members++] = t;
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

static const char *type_set_fmt(void)
{
   static char buf[1024];
   static_printf_begin(buf, sizeof(buf));
   if (top_type_set != NULL) {
      for (unsigned n = 0; n < top_type_set->n_members; n++)
         static_printf(buf, "\n    %s",
                       sem_type_str(top_type_set->members[n]));
   }
   return buf;
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

static void sem_add_port(tree_t d, type_t type, port_mode_t mode, tree_t def)
{
   type_t ftype = tree_type(d);

   char *argname = xasprintf("_arg%d", type_params(ftype));
   tree_t port = tree_new(T_PORT_DECL);
   tree_set_ident(port, ident_new(argname));
   free(argname);
   tree_set_type(port, type);
   tree_set_subkind(port, mode);
   if (def != NULL)
      tree_set_value(port, def);

   tree_add_port(d, port);
   type_add_param(ftype, type);
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

static void sem_declare_binary(ident_t name, type_t lhs, type_t rhs,
                               type_t result, const char *builtin)
{
   tree_t d = sem_builtin_fn(name, result, builtin, lhs, rhs, NULL);
   scope_insert(d);
}

static void sem_declare_unary(ident_t name, type_t operand,
                              type_t result, const char *builtin)
{
   tree_t d = sem_builtin_fn(name, result, builtin, operand, NULL);
   scope_insert(d);
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

static void sem_add_dimension_attr(tree_t decl, const char *name,
                                   const char *builtin)
{
   ident_t length_i = ident_new(name);
   type_t std_int = sem_std_type("INTEGER");
   tree_t fn = sem_builtin_fn(length_i, std_int, builtin,
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
      sem_declare_binary(ident_new("\"=\""), t, t, std_bool, "aeq");
      sem_declare_binary(ident_new("\"/=\""), t, t, std_bool, "aneq");
      if (sem_array_dimension(t) == 1) {
         sem_declare_binary(ident_new("\"<\""), t, t, std_bool, "alt");
         sem_declare_binary(ident_new("\"<=\""), t, t, std_bool, "aleq");
         sem_declare_binary(ident_new("\">\""), t, t, std_bool, "agt");
         sem_declare_binary(ident_new("\">=\""), t, t, std_bool, "ageq");
      }
      break;

   case T_RECORD:
      // Operators on records
      sem_declare_binary(ident_new("\"=\""), t, t, std_bool, "req");
      sem_declare_binary(ident_new("\"/=\""), t, t, std_bool, "rneq");
      break;

   case T_PHYSICAL:
      // Multiplication
      sem_declare_binary(mult, t, std_int, t, "mul");
      sem_declare_binary(mult, t, std_real, t, "mulpr");
      sem_declare_binary(mult, std_int, t, t, "mul");
      sem_declare_binary(mult, std_real, t, t, "mulrp");

      // Division
      sem_declare_binary(div, t, std_int, t, "div");
      sem_declare_binary(div, t, std_real, t, "divpr");
      sem_declare_binary(div, t, t, std_int, "div");

      // Addition
      sem_declare_binary(plus, t, t, t, "add");

      // Subtraction
      sem_declare_binary(minus, t, t, t, "sub");

      // Sign operators
      sem_declare_unary(plus, t, t, "identity");
      sem_declare_unary(minus, t, t, "neg");

      // Comparison
      sem_declare_binary(ident_new("\"<\""), t, t, std_bool, "lt");
      sem_declare_binary(ident_new("\"<=\""), t, t, std_bool, "leq");
      sem_declare_binary(ident_new("\">\""), t, t, std_bool, "gt");
      sem_declare_binary(ident_new("\">=\""), t, t, std_bool, "geq");

      // Equality
      sem_declare_binary(ident_new("\"=\""), t, t, std_bool, "eq");
      sem_declare_binary(ident_new("\"/=\""), t, t, std_bool, "neq");

      // Absolute value
      sem_declare_unary(ident_new("\"abs\""), t, t, "abs");

      break;

   case T_INTEGER:
      // Modulus
      sem_declare_binary(ident_new("\"mod\""), t, t, t, "mod");

      // Remainder
      sem_declare_binary(ident_new("\"rem\""), t, t, t, "rem");

      // Fall-through
   case T_REAL:
      // Addition
      sem_declare_binary(plus, t, t, t, "add");

      // Subtraction
      sem_declare_binary(minus, t, t, t, "sub");

      // Multiplication
      sem_declare_binary(mult, t, t, t, "mul");

      // Division
      sem_declare_binary(div, t, t, t, "div");

      // Sign operators
      sem_declare_unary(plus, t, t, "identity");
      sem_declare_unary(minus, t, t, "neg");

      // Exponentiation
      sem_declare_binary(ident_new("\"**\""), t, std_int, t, "exp");

      // Absolute value
      sem_declare_unary(ident_new("\"abs\""), t, t, "abs");

      // Fall-through
   case T_ENUM:
      sem_declare_binary(ident_new("\"<\""), t, t, std_bool, "lt");
      sem_declare_binary(ident_new("\"<=\""), t, t, std_bool, "leq");
      sem_declare_binary(ident_new("\">\""), t, t, std_bool, "gt");
      sem_declare_binary(ident_new("\">=\""), t, t, std_bool, "geq");

      // Fall-through
   default:
      sem_declare_binary(ident_new("\"=\""), t, t, std_bool, "eq");
      sem_declare_binary(ident_new("\"/=\""), t, t, std_bool, "neq");

      break;
   }

   // Logical operators

   ident_t boolean_i = ident_new("STD.STANDARD.BOOLEAN");
   ident_t bit_i = ident_new("STD.STANDARD.BIT");

   bool logical = (type_ident(t) == boolean_i || type_ident(t) == bit_i);

   if (logical) {
      sem_declare_binary(ident_new("\"and\""), t, t, t, "and");
      sem_declare_binary(ident_new("\"or\""), t, t, t, "or");
      sem_declare_binary(ident_new("\"xor\""), t, t, t, "xor");
      sem_declare_binary(ident_new("\"nand\""), t, t, t, "nand");
      sem_declare_binary(ident_new("\"nor\""), t, t, t, "nor");
      sem_declare_binary(ident_new("\"xnor\""), t, t, t, "xnor");
      sem_declare_unary(ident_new("\"not\""), t, t, "not");
   }

   bool vec_logical = false;
   if (kind == T_CARRAY || kind == T_UARRAY) {
      type_t base = type_elem(t);
      vec_logical = (type_ident(base) == boolean_i
                     || type_ident(base) == bit_i);
   }

   if (vec_logical) {
      sem_declare_binary(ident_new("\"and\""), t, t, t, "v_and");
      sem_declare_binary(ident_new("\"or\""), t, t, t, "v_or");
      sem_declare_binary(ident_new("\"xor\""), t, t, t, "v_xor");
      sem_declare_binary(ident_new("\"nand\""), t, t, t, "v_nand");
      sem_declare_binary(ident_new("\"nor\""), t, t, t, "v_nor");
      sem_declare_binary(ident_new("\"xnor\""), t, t, t, "v_xnor");
      sem_declare_unary(ident_new("\"not\""), t, t, "v_not");

      sem_declare_binary(ident_new("\"sll\""), t, std_int, t, "sll");
      sem_declare_binary(ident_new("\"srl\""), t, std_int, t, "srl");
      sem_declare_binary(ident_new("\"sla\""), t, std_int, t, "sla");
      sem_declare_binary(ident_new("\"sra\""), t, std_int, t, "sra");
      sem_declare_binary(ident_new("\"rol\""), t, std_int, t, "rol");
      sem_declare_binary(ident_new("\"ror\""), t, std_int, t, "ror");
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

         tree_t file_open2 = sem_builtin_proc(file_open_i, "file_open2");
         sem_add_port(file_open2, open_status, PORT_OUT, NULL);
         sem_add_port(file_open2, t, PORT_INOUT, NULL);
         sem_add_port(file_open2, std_string, PORT_IN, NULL);
         sem_add_port(file_open2, open_kind, PORT_IN, make_ref(read_mode));
         scope_insert(file_open2);

         tree_t file_close = sem_builtin_proc(file_close_i, "file_close");
         sem_add_port(file_close, t, PORT_INOUT, NULL);
         scope_insert(file_close);

         tree_t read = sem_builtin_proc(read_i, "file_read");
         sem_add_port(read, t, PORT_INOUT, NULL);
         sem_add_port(read, of, PORT_OUT, NULL);
         if (type_is_array(of))
            sem_add_port(read, std_int, PORT_OUT, NULL);
         scope_insert(read);

         tree_t write = sem_builtin_proc(write_i, "file_write");
         sem_add_port(write, t, PORT_INOUT, NULL);
         sem_add_port(write, of, PORT_IN, NULL);
         scope_insert(write);

         sem_declare_unary(endfile_i, t, std_bool, "endfile");
      }
      break;

   case T_ACCESS:
      {
         ident_t deallocate_i = ident_new("DEALLOCATE");

         tree_t deallocate = sem_builtin_proc(deallocate_i, "deallocate");
         sem_add_port(deallocate, t, PORT_INOUT, NULL);
         scope_insert(deallocate);
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
      sem_add_dimension_attr(decl, "LENGTH", "length");
      sem_add_dimension_attr(decl, "LEFT", "left");
      sem_add_dimension_attr(decl, "RIGHT", "right");
      sem_add_dimension_attr(decl, "LOW", "low");
      sem_add_dimension_attr(decl, "HIGH", "high");
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
            ? sem_array_dimension(base)
            : ((base_kind == T_SUBTYPE) ? type_dims(base) : 1);

         if (ndims != ndims_base)
            sem_error(t, "expected %d constraints for type %s but found %d",
                      ndims_base, sem_type_str(base), ndims);

         for (int i = 0; i < ndims; i++) {
            range_t r = type_dim(type, i);
            if (!sem_check_range(&r, sem_index_type(base, i)))
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

static bool sem_declare(tree_t decl)
{
   // Handle special cases of scope insertion such as enumeration
   // literals, physical unit names, and predefined types

   // Certain kinds of declarations like components do not have
   // a type
   if (tree_kind(decl) == T_COMPONENT)
      return true;

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
   sem_declare_predefined_ops(decl);

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
      const bool reverse = (r->left == NULL);

      tree_t expr = reverse ? r->right : r->left;

      assert(tree_kind(expr) == T_ATTR_REF
             || tree_kind(expr) == T_REF);

      ident_t name =
         (tree_kind(expr) == T_ATTR_REF)
         ? tree_ident(tree_name(expr))
         : tree_ident(expr);

      tree_t decl = scope_find(name);
      if (decl == NULL)
         sem_error(expr, "undefined identifier %s", istr(name));

      tree_t a = tree_new(T_ATTR_REF);
      tree_set_name(a, make_ref(decl));
      tree_set_ident(a, ident_new("LEFT"));
      tree_set_loc(a, tree_loc(expr));

      tree_t b = tree_new(T_ATTR_REF);
      tree_set_name(b, make_ref(decl));
      tree_set_ident(b, ident_new("RIGHT"));
      tree_set_loc(b, tree_loc(expr));

      type_t type = tree_type(decl);
      type_kind_t kind = type_kind(type);
      switch (kind) {
      case T_CARRAY:
      case T_SUBTYPE:
         {
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
         break;
      case T_ENUM:
      case T_UARRAY:
         {
            // If this is an unconstrained array then we can
            // only find out the direction at runtime
            r->kind  = (kind == T_UARRAY
                        ? (reverse ? RANGE_RDYN : RANGE_DYN)
                        : (reverse ? RANGE_DOWNTO : RANGE_TO));
            r->left  = a;
            r->right = b;
         }
         break;
      default:
         sem_error(r->left, "object %s does not have a range",
                   istr(tree_ident(r->left)));
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
      // See LRM 93 section 3.2.11
      if (type_is_universal(left_type) && type_is_universal(right_type)) {
         tree_kind_t lkind = tree_kind(r->left);
         tree_kind_t rkind = tree_kind(r->right);

         if ((lkind != T_LITERAL) && (lkind != T_ATTR_REF)
             && (rkind != T_LITERAL) && (rkind != T_ATTR_REF))
            sem_error(r->left, "universal integer bound must be "
                      "numeric literal or attribute");

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

   const bool all = tree_has_ident2(c) && (icmp(tree_ident2(c), "all"));

   ident_t lname = ident_until(cname, '.');

   lib_t lib = lib_find(istr(lname), true, true);
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

            tree_t object = scope_find(full);
            if (object == NULL)
               sem_error(c, "declaration %s not found in unit %s",
                         istr(tree_ident2(c)), istr(cname));
            else
               scope_insert_alias(object, tree_ident2(c));
         }

         return true;
      }
      else
         return false;
   }
   else {
      errors++;
      return false;
   }
}

static void sem_declare_universal(void)
{
   // Universal integers and reals have some additional overloaded operators
   // that are not valid for regular integer and and real types
   // See LRM 93 section 7.5

   type_t uint  = type_universal_int();
   type_t ureal = type_universal_real();

   ident_t mult = ident_new("\"*\"");
   ident_t div  = ident_new("\"/\"");

   sem_declare_binary(mult, ureal, uint, ureal, "mulri");
   sem_declare_binary(mult, uint, ureal, ureal, "mulir");
   sem_declare_binary(div, ureal, uint, ureal, "divri");
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
      ok = sem_check_use_clause(tree_context(t, n)) && ok;

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
         if (tree_kind(decl) == T_PORT_DECL
             && tree_subkind(decl) == PORT_OUT)
            sem_error(t, "cannot read output port %s",
                      istr(tree_ident(t)));

         return true;
      }

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

         if (tree_kind(type_decl) != T_TYPE_DECL)
            sem_error(t, "name %s does not refer to a type",
                      istr(tree_ident(type_decl)));

         *ptype = tree_type(type_decl);
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
   if (!sem_declare(t))
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

   switch (type_kind(type)) {
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

   switch (type_kind(type)) {
   case T_CARRAY:
      return sem_check_array_dims(base, NULL);

   case T_UARRAY:
      for (unsigned i = 0; i < type_index_constrs(type); i++) {
         type_t index_type = type_index_constr(type, i);
         if (!sem_check_type(t, &index_type))
            return false;

         type_change_index_constr(type, i, index_type);
      }
      return true;

   case T_PHYSICAL:
      {
         // Check the units
         const int nunits = type_units(type);
         for (int i = 0; i < nunits; i++) {
            if (!sem_check_constrained(type_unit(type, i), type))
               return false;
         }
      }

      // Fall-through
   case T_INTEGER:
   case T_REAL:
      {
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

            type_set_push();

            type_t index = NULL;
            switch (type_kind(base)) {
            case T_CARRAY:
               index = tree_type(type_dim(base, i).left);
               break;
            case T_UARRAY:
               index = type_index_constr(base, i);
               break;
            case T_PHYSICAL:
               type_set_add(sem_std_type("INTEGER"));
               // Fall-through
            default:
               index = base;
               break;
            }

            if (type_kind(index) == T_UNRESOLVED)
               return false;

            type_set_add(index);

            ok = sem_check(r.left) && sem_check(r.right) && ok;
            type_set_pop();

            if (ok) {
               tree_set_type(r.left, index);
               tree_set_type(r.right, index);
            }
         }

         if (type_has_resolution(type))
            ok = ok && sem_check_resolution(type);

         return ok;
      }

   case T_RECORD:
      {
         const int nfields = type_fields(type);
         for (int i = 0; i < nfields; i++) {
            tree_t f = type_field(type, i);

            if (!sem_check(f))
               return false;

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

         return true;
      }

   case T_ACCESS:
      // Rules for access types are in LRM 93 section 3.3
      {
         type_t a = type_access(base);

         if (!sem_check_type(t, &a))
            return false;
         type_set_access(base, a);

         return true;
      }

   default:
      return true;
   }
}

static void sem_add_attributes(tree_t decl)
{
   type_t std_bool = sem_std_type("BOOLEAN");

   type_t type;
   tree_kind_t kind = tree_kind(decl);
   if ((kind != T_ARCH) && (kind != T_ENTITY) && (kind != T_COMPONENT))
      type = tree_type(decl);
   else
      type = type_new(T_NONE);

   // Implicit dereference for access types
   if (type_kind(type) == T_ACCESS)
      type = type_access(type);

   if (type_is_array(type)) {
      sem_add_dimension_attr(decl, "LENGTH", "length");
      sem_add_dimension_attr(decl, "LEFT", "left");
      sem_add_dimension_attr(decl, "RIGHT", "right");
      sem_add_dimension_attr(decl, "LOW", "low");
      sem_add_dimension_attr(decl, "HIGH", "high");

      // TODO: 'ASCENDING should also take dimension argument
      if (type_is_unconstrained(type)) {
         ident_t asc_i = ident_new("ASCENDING");
         tree_add_attr_tree(decl, asc_i,
                            sem_builtin_fn(asc_i, std_bool,
                                           "uarray_asc", type, NULL));
      }
      else {
         range_t r = type_dim(type, 0);

         if ((r.kind != RANGE_DYN) && (r.kind != RANGE_RDYN))
            tree_add_attr_tree(decl, ident_new("ASCENDING"),
                               sem_bool_lit(std_bool, r.kind == RANGE_TO));
         else {
            ident_t asc_i = ident_new("ASCENDING");
            tree_add_attr_tree(decl, asc_i,
                               sem_builtin_fn(asc_i, std_bool,
                                              "uarray_asc", type, NULL));
         }
      }
   }

   const tree_kind_t decl_kind = tree_kind(decl);

   const bool is_signal =
      (decl_kind == T_PORT_DECL && tree_class(decl) == C_SIGNAL)
      || (decl_kind == T_SIGNAL_DECL);

   if (is_signal) {
      type_t std_time   = sem_std_type("TIME");

      ident_t event_i      = ident_new("EVENT");
      ident_t last_value_i = ident_new("LAST_VALUE");
      ident_t active_i     = ident_new("ACTIVE");
      ident_t last_event_i = ident_new("LAST_EVENT");

      tree_add_attr_tree(decl, event_i,
                         sem_builtin_fn(event_i, std_bool, "event",
                                        type, NULL));
      tree_add_attr_tree(decl, active_i,
                         sem_builtin_fn(active_i, std_bool, "active",
                                        type, NULL));
      tree_add_attr_tree(decl, last_value_i,
                         sem_builtin_fn(last_value_i, type, "last_value",
                                        type, NULL));
      tree_add_attr_tree(decl, last_event_i,
                         sem_builtin_fn(last_event_i, std_time,
                                        "last_event", type, NULL));
   }

   if (is_signal || (decl_kind == T_ARCH) || (decl_kind == T_ENTITY)
       || (decl_kind == T_COMPONENT) || (decl_kind == T_FUNC_DECL)
       || (decl_kind == T_FUNC_BODY) || (decl_kind == T_PROC_DECL)
       || (decl_kind == T_PROC_BODY)) {
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

static type_t sem_array_aggregate_type(type_t array, int from_dim)
{
   const int ndims = type_dims(array);

   assert(from_dim < ndims);

   type_t type = type_new(T_CARRAY);
   type_set_ident(type, type_ident(array));
   type_set_elem(type, type_elem(array));

   for (int i = from_dim; i < ndims; i++)
      type_add_dim(type, type_dim(array, i));

   return type;
}

static tree_t sem_default_value(type_t type)
{
   type_t base = type_base_recur(type);

   switch (type_kind(base)) {
   case T_UARRAY:
      assert(type_kind(type) == T_SUBTYPE);
      // Fall-through

   case T_CARRAY:
      {
         tree_t def = NULL;
         const int ndims = type_dims(type);
         for (int i = ndims - 1; i >= 0; i--) {
            tree_t val = (def ? def : sem_default_value(type_elem(base)));
            def = tree_new(T_AGGREGATE);
            tree_set_type(def, sem_array_aggregate_type(type, i));

            tree_t a = tree_new(T_ASSOC);
            tree_set_subkind(a, A_OTHERS);
            tree_set_value(a, val);

            tree_add_assoc(def, a);
         }
         tree_set_type(def, type);
         return def;
      }

   case T_INTEGER:
   case T_PHYSICAL:
   case T_REAL:
      return type_dim(type, 0).left;

   case T_ENUM:
      return make_ref(type_enum_literal(base, 0));

   case T_RECORD:
      {
         tree_t def = tree_new(T_AGGREGATE);
         const int nfields = type_fields(base);
         for (int i = 0; i < nfields; i++) {
            tree_t field = type_field(base, i);

            tree_t a = tree_new(T_ASSOC);
            tree_set_subkind(a, A_POS);
            tree_set_value(a, sem_default_value(tree_type(field)));

            tree_add_assoc(def, a);
         }
         tree_set_type(def, type);
         return def;
      }

   case T_ACCESS:
      {
         tree_t null = tree_new(T_LITERAL);
         tree_set_subkind(null, L_NULL);
         tree_set_type(null, type);
         return null;
      }

   default:
      assert(false);
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

static bool sem_check_decl(tree_t t)
{
   type_t type = tree_type(t);
   if (!sem_check_type(t, &type))
      return false;

   tree_set_type(t, type);

   tree_kind_t kind = tree_kind(t);

   if (!tree_has_value(t) && (kind == T_CONST_DECL) && !top_scope->is_package)
      sem_error(t, "deferred constant declarations are only permitted "
                "in packages");

   if (type_is_unconstrained(type) && (kind != T_CONST_DECL))
      sem_error(t, "type %s is unconstrained", sem_type_str(type));

   if (!tree_has_value(t) && (kind != T_PORT_DECL) && (kind != T_CONST_DECL))
      tree_set_value(t, sem_default_value(type));
   else if (tree_has_value(t)) {
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
   }

   if (kind == T_PORT_DECL && tree_class(t) == C_DEFAULT)
      tree_set_class(t, C_SIGNAL);

   if (type_is_record(type)) {
      sem_declare_fields(type, tree_ident(t));
   }
   else if (type_kind(type) == T_ACCESS) {
      type_t deref_type = type_access(type);
      if (type_is_record(deref_type)) {
         // Pointers to records can be dereferenced implicitly
         sem_declare_fields(deref_type, tree_ident(t));
      }

      if (kind == T_SIGNAL_DECL)
         sem_error(t, "signals may not have access type");
   }

   sem_add_attributes(t);
   scope_apply_prefix(t);

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

   sem_add_attributes(t);
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

   if (!sem_check(value))
      return false;

   if (!sem_static_name(value))
      sem_error(value, "aliased name is not static");

   if (tree_has_type(t)) {
      type_t type = tree_type(t);
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

   sem_add_attributes(t);

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

static bool sem_check_func_ports(tree_t t)
{
   type_t ftype = tree_type(t);

   const int nports = tree_ports(t);
   for (int i = 0; i < nports; i++) {
      tree_t p = tree_port(t, i);
      if (tree_subkind(p) != PORT_IN)
         sem_error(p, "function arguments must have mode IN");

      // See LRM 93 section 2.1.1 for default class
      class_t class = tree_class(p);
      if (class == C_DEFAULT)
         tree_set_class(p, (class = C_CONSTANT));
      else if (class == C_VARIABLE)
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
      ok = sem_check(s) && ok;
   }

   // Check for duplicate statements: if the number of statements is small
   // then it is simpler to do this with the naive O(n^2) algorithm but when
   // the number is large it is more efficient to use a hash table

   const bool use_hash = (nstmts >= 128);
   hash_t *hash = NULL;
   if (use_hash)
      hash = hash_new(nstmts * 2, true);

   for (int i = 0; i < nstmts; i++) {
      tree_t s = get_stmt(t, i);
      ident_t label = tree_ident(s);

      bool duplicate = false;
      if (use_hash)
         duplicate = hash_put(hash, label, NULL);
      else {
         for (int j = 0; (j < i) && !duplicate; j++) {
            if (tree_ident(get_stmt(t, j)) == label)
               duplicate = true;
         }
      }

      if (duplicate)
         sem_error(s, "duplicate statement label %s", istr(label));
   }

   if (use_hash)
      hash_free(hash);

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

   sem_add_attributes(t);

   scope_apply_prefix(t);
   return scope_insert(t);
}

static bool sem_check_func_body(tree_t t)
{
   if (!sem_check_func_ports(t))
      return false;

   ident_t unqual = top_scope->prefix ? tree_ident(t) : NULL;
   scope_apply_prefix(t);

   sem_add_attributes(t);

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
      sem_add_attributes(p);
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

   return true;
}

static bool sem_check_proc_decl(tree_t t)
{
   if (!sem_check_proc_ports(t))
      return false;

   if (sem_check_duplicate(t, T_PROC_DECL))
      sem_error(t, "duplicate declaration of procedure %s",
                istr(tree_ident(t)));

   sem_add_attributes(t);

   scope_apply_prefix(t);
   return scope_insert(t);
}

static bool sem_check_proc_body(tree_t t)
{
   if (!sem_check_proc_ports(t))
      return false;

   scope_apply_prefix(t);

   sem_add_attributes(t);

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
      sem_add_attributes(p);
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
      top_scope->is_package = true;

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

   if (ok && (pack != NULL) && !opt_get_int("unit-test")) {
      // Check for any subprogram declarations without bodies
      const int ndecls = tree_decls(pack);
      for (int i = 0; i < ndecls; i++) {
         tree_t d = tree_decl(pack, i);
         tree_kind_t dkind = tree_kind(d);
         if ((dkind == T_FUNC_DECL) || (dkind == T_PROC_DECL)) {
            type_t dtype = tree_type(d);

            bool found = false;
            const int nbody_decls = tree_decls(t);
            for (int j = 0; !found && (j < nbody_decls); j++) {
               tree_t b = tree_decl(t, j);
               tree_kind_t bkind = tree_kind(b);
               if ((bkind == T_FUNC_BODY) || (bkind == T_PROC_BODY)) {
                  if (type_eq(dtype, tree_type(b)))
                     found = true;
               }
            }

            if (!found)
               warn_at(tree_loc(d), "missing body for %s %s",
                       (dkind == T_FUNC_DECL) ? "function" : "procedure",
                       sem_type_str(dtype));
         }
      }
   }

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

   sem_add_attributes(t);

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

   sem_add_attributes(t);

   if (ok) {
      const int ndecls = tree_decls(t);
      for (int n = 0; n < ndecls; n++)
         ok = sem_check(tree_decl(t, n)) && ok;

      ok = ok && sem_check_stmts(t, tree_stmt, tree_stmts(t));
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

   sem_add_attributes(t);

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
         sem_declare(d);
   }

   // Now check the architecture itself

   const int ndecls = tree_decls(t);
   for (int n = 0; n < ndecls; n++)
      ok = sem_check(tree_decl(t, n)) && ok;

   ok = ok && sem_check_stmts(t, tree_stmt, tree_stmts(t));

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

   if (!type_eq(tree_type(target), tree_type(value)))
      sem_error(t, "type of value %s does not match type of target %s",
                sem_type_str(tree_type(value)),
                sem_type_str(tree_type(target)));

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

      if (!type_eq(expect, tree_type(value)))
         sem_error(t, "type of value %s does not match type of target %s",
                   sem_type_str(tree_type(value)),
                   sem_type_str(expect));

      if (tree_has_delay(waveform)) {
         tree_t delay = tree_delay(waveform);
         if (!sem_check(delay))
            return false;

         if (!type_eq(tree_type(delay), std_time))
            sem_error(delay, "type of delay must be %s", sem_type_str(std_time));
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

         if (!sem_check_signal_target(tree_value(a)))
            return false;

         assoc_kind_t kind = tree_subkind(a);
         switch (kind) {
         case A_OTHERS:
            sem_error(a, "others association not allowed in aggregate "
                      "signal target");
         case A_RANGE:
            sem_error(a, "range association not allowd in aggregate "
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
                      istr(tree_ident(target)));
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

         if (!sem_check(test))
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

static unsigned sem_array_dimension(type_t a)
{
   return (type_is_unconstrained(a)
           ? type_index_constrs(type_base_recur(a))
           : type_dims(a));
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
      bool same_dim = (sem_array_dimension(from) == sem_array_dimension(to));

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
   case T_LITERAL:
      return (tree_subkind(t) == L_NULL) ? 0 : -10;
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
         match = match && type_eq(mtype, ptype);
         all_universal = all_universal && type_is_universal(ptype);
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
      if (!tree_has_value(tree_port(decl, i)))
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
         if (tree_has_value(port)) {
            tree_t value = tree_value(port);

            tree_t p = tree_new(T_PARAM);
            tree_set_subkind(p, P_NAMED);
            tree_set_loc(p, tree_loc(value));
            tree_set_value(p, value);
            tree_set_name(p, make_ref(port));

            tree_add_param(call, p);

            found = p;
         }
         else
            sem_error(call, "missing actual for formal %s without "
                      "default value", istr(name));
      }

      // Check IN and INOUT parameters can be read
      if (tree_kind(call) != T_ATTR_REF) {
         port_mode_t mode = tree_subkind(port);
         if ((mode == PORT_IN) || (mode == PORT_INOUT)) {
            if (!sem_readable(tree_value(found)))
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

   tree_t overloads[MAX_OVERLOADS];
   int n_overloads = 0;

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
         default:
            if (type_is_array(func_type)
                || ((type_kind(func_type) == T_ACCESS)
                    && type_is_array(type_access(func_type)))) {
               // The grammar is ambiguous between function calls and
               // array references so must be an array reference
               tree_t ref = tree_new(T_REF);
               tree_set_ident(ref, name);

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
                     && opt_get_int("prefer-explicit");

                  if (same_name || hide_implicit)
                     duplicate = true;
               }
            }

            if (!duplicate) {
               // Found a matching function definition
               assert(n_overloads < MAX_OVERLOADS);
               overloads[n_overloads++] = decl;
            }
         }
      }
   } while (decl != NULL);

   if (n_overloads == 0)
      sem_error(t, (found_func > 0
                    ? "no matching function %s"
                    : "undefined identifier %s"),
                istr(name));

   int matches;
   if (!sem_resolve_overload(t, &decl, &matches, overloads, n_overloads))
      return false;

   if (matches > 0 && decl == NULL)
      return true;   // Resolved to a builtin function

   if (matches > 1) {
      char buf[1024];
      static_printf_begin(buf, sizeof(buf));

      const bool operator = !isalpha((int)*istr(name));

      int nimplict = 0, nexplict = 0;
      for (int n = 0; n < n_overloads; n++) {
         if (overloads[n] != NULL) {
            const bool implicit = tree_attr_str(overloads[n], builtin_i);
            static_printf(buf, "\n    %s%s",
                          sem_type_str(tree_type(overloads[n])),
                          implicit ? " (implicit)" : "");
            if (implicit)
               nimplict++;
            else
               nexplict++;
         }
      }

      if ((nimplict == 1) && (nexplict == 1))
         static_printf(buf, "\nYou can use the --prefer-explicit option to "
                       "hide the implicit %s",
                       operator ? "operator" : "function");

      sem_error(t, "ambiguous %s %s%s",
                operator ? "use of operator" : "call to function",
                istr(name), buf);
   }

   if (decl == NULL) {
      char fn[512];
      static_printf_begin(fn, sizeof(fn));

      const char *fname = istr(name);
      const bool operator = !isalpha((int)fname[0]);
      const char *quote = (operator && fname[0] != '"') ? "\"" : "";

      static_printf(fn, "%s%s%s(", quote, fname, quote);
      for (unsigned i = 0; i < tree_params(t); i++)
         static_printf(fn, "%s%s",
                       (i == 0 ? "" : ", "),
                       sem_type_str(tree_type(tree_value(tree_param(t, i)))));
      static_printf(fn, ")");

      if ((top_type_set != NULL) && (top_type_set->n_members > 0)) {
         static_printf(fn, " return");
         for (int i = 0; i < top_type_set->n_members; i++)
            static_printf(fn, "%s %s",
                          (i > 0 ? " |" : ""),
                          sem_type_str(top_type_set->members[i]));
      }

      sem_error(t, (n == 1 ? "undefined %s %s"
                    : "no suitable overload for %s %s"),
                operator ? "operator" : "function",
                fn);
   }

   // Pure function may not call an impure function
   tree_t sub = top_scope->subprog;
   if ((sub != NULL) && (tree_kind(sub) == T_FUNC_BODY)) {
      ident_t impure_i = ident_new("impure");
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

   tree_t overloads[MAX_OVERLOADS];
   int n_overloads = 0;

   tree_t decl;
   int n = 0, found_proc = 0;
   do {
      if ((decl = scope_find_nth(tree_ident2(t), n++))) {
         switch (tree_kind(decl)) {
         case T_PROC_DECL:
         case T_PROC_BODY:
            found_proc++;
            break;
         default:
            continue;   // Look for the next matching name
         }

         // Number of arguments must match
         if (!sem_check_arity(t, decl))
            continue;

         // Found a matching function definition
         overloads[n_overloads++] = decl;
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
      char buf[1024];
      static_printf_begin(buf, sizeof(buf));

      for (int n = 0; n < n_overloads; n++) {
         if (overloads[n] != NULL)
            static_printf(buf, "\n    %s",
                          sem_type_str(tree_type(overloads[n])));
      }

      sem_error(t, "ambiguous call to procedure %s%s",
                istr(tree_ident2(t)), buf);
   }

   if (decl == NULL) {
      char fn[512];
      static_printf_begin(fn, sizeof(fn));

      const char *fname = istr(tree_ident2(t));

      static_printf(fn, "%s(", fname);
      for (unsigned i = 0; i < tree_params(t); i++)
         static_printf(fn, "%s%s",
                       (i == 0 ? "" : ", "),
                       sem_type_str(tree_type(tree_value(tree_param(t, i)))));
      static_printf(fn, ")");

      sem_error(t, (n == 1 ? "undefined procedure %s"
                    : "no suitable overload for procedure %s"),
                fn);
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
            if ((mode == PORT_OUT) && (tree_subkind(decl) != PORT_OUT))
               sem_error(value, "cannot read parameter %s with mode OUT",
                         istr(tree_ident(decl)));
            else if (((mode == PORT_OUT) || (mode == PORT_INOUT))
                     && (tree_class(decl) == C_CONSTANT))
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

   tree_t value  = tree_value(t);
   tree_t severity = tree_severity(t);
   tree_t message  = tree_message(t);

   if (!sem_check_constrained(value, std_bool))
      return false;

   if (!sem_check_constrained(severity, std_severity))
      return false;

   if (!sem_check_constrained(message, std_string))
      return false;

   if (!type_eq(tree_type(value), std_bool))
      sem_error(value, "type of assertion expression must "
                "be %s but is %s", sem_type_str(std_bool),
                sem_type_str(tree_type(value)));

   if (!type_eq(tree_type(severity), std_severity))
      sem_error(severity, "type of severity must be %s but is %s",
                sem_type_str(std_severity),
                sem_type_str(tree_type(severity)));

   if (!type_eq(tree_type(message), std_string))
      sem_error(message, "type of message be %s but is %s",
                sem_type_str(std_string),
                sem_type_str(tree_type(message)));

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

static type_t sem_index_type(type_t type, int dim)
{
   if (type_is_unconstrained(type))
      return type_index_constr(type_base_recur(type), dim);
   else if (type_kind(type) == T_ENUM)
      return type;
   else {
      tree_t left = type_dim(type, dim).left;
      return tree_has_type(left) ? tree_type(left) : type;
   }
}

static bool sem_check_concat_param(tree_t t, type_t hint)
{
   struct type_set *old = top_type_set;

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

   if ((top_type_set->n_members > 0) && !type_set_restrict(sem_is_composite))
      sem_error(t, "no composite type in context%s", type_set_fmt());

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

      if (sem_array_dimension(ltype) > 1)
         sem_error(t, "cannot concatenate arrays with more than one dimension");

      type_t index_type = sem_index_type(ltype, 0);
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

      if (sem_array_dimension(atype) > 1)
         sem_error(t, "cannot concatenate arrays with more than one dimension");

      if (!type_eq(stype, type_elem(atype)))
         sem_error(t, "type of scalar does not match element type of array");

      type_t index_type = sem_index_type(atype, 0);
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

      type_t index_type = sem_index_type(composite, 0);
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

   default:
      assert(false);
   }

   return true;
}

static bool sem_check_uarray_aggregate(tree_t t, type_t type,
                                       int dim, int *n_elems)
{
   const int nassocs = tree_assocs(t);
   const int ndims   = sem_array_dimension(type);

   for (int i = 0; i < nassocs; i++) {
      tree_t a = tree_assoc(t, i);

      if (tree_subkind(a) == A_OTHERS)
         sem_error(tree_value(a), "OTHERS choice not allowed in unconstrained "
                   "array aggregate");

      if (dim + 1 < ndims) {
         if (!sem_check_uarray_aggregate(tree_value(a), type, dim + 1, n_elems))
            return false;
      }
   }

   if (n_elems[dim] == -1) {
      // First aggregate in this dimension determines size
      n_elems[dim] = nassocs;
   }
   else if (n_elems[dim] != nassocs)
      sem_error(t, "aggregate size mismatch in dimension %d", dim);

   return true;
}

static bool sem_check_aggregate(tree_t t)
{
   // Rules for aggregates are in LRM 93 section 7.3.2

   // The type of an aggregate must be determinable solely from the
   // context in which the aggregate appears

   if (!type_set_restrict(sem_is_composite))
      sem_error(t, "no composite type in context%s", type_set_fmt());

   type_t composite_type;
   if (!type_set_uniq(&composite_type))
      sem_error(t, "type of aggregate is ambiguous%s", type_set_fmt());

   type_t base_type = composite_type;
   while (type_kind(base_type) == T_SUBTYPE)
      base_type = type_base(base_type);

   // All positional associations must appear before named associations
   // and those must appear before any others association

   enum { POS, NAMED, OTHERS } state = POS;
   bool have_named = false;
   bool have_pos = false;

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
         if (type_is_unconstrained(composite_type))
            sem_error(a, "others choice not allowed in this context");
         state = OTHERS;
         break;
      }
   }

   // Named and positional associations cannot be mixed in array
   // aggregates

   if (type_is_array(base_type) && have_named && have_pos)
      sem_error(t, "named and positional associations cannot be "
                "mixed in array aggregates");

   // If the composite type is unconstrained create a new constrained
   // array type

   if (type_is_unconstrained(composite_type)) {
      const int nindex = sem_array_dimension(composite_type);

      int n_elems[nindex];
      for (int i = 0; i < nindex; i++)
         n_elems[i] = -1;

      if (!sem_check_uarray_aggregate(t, composite_type, 0, n_elems))
         return false;

      type_t tmp = type_new(T_SUBTYPE);
      type_set_ident(tmp, type_ident(composite_type));
      type_set_base(tmp, composite_type);

      for (int i = 0; i < nindex; i++) {
         type_t index_type = sem_index_type(composite_type, i);
         range_t index_r = type_dim(index_type, 0);

         if (have_named) {
            tree_t low = call_builtin("agg_low", index_type, t, NULL);
            tree_t high = call_builtin("agg_high", index_type, t, NULL);

            range_t r = {
               .kind  = index_r.kind,
               .left  = (index_r.kind == RANGE_TO ? low : high),
               .right = (index_r.kind == RANGE_TO ? high : low)
            };
            type_add_dim(tmp, r);
         }
         else {
            type_t std_int = sem_std_type("INTEGER");
            range_t r = {
               .kind  = index_r.kind,
               .left  = index_r.left,
               .right = call_builtin("add", index_type,
                                     sem_int_lit(std_int, n_elems[i] - 1),
                                     index_r.left, NULL)
            };
            type_add_dim(tmp, r);
         }
      }

      tree_add_attr_int(t, ident_new("unconstrained"), 1);

      composite_type = tmp;
   }

   // All elements must be of the composite base type if this is
   // a one-dimensional array otherwise construct an array type
   // with n-1 dimensions.

   if (type_is_array(base_type)) {
      type_t elem_type = NULL;
      const int ndims = type_dims(composite_type);
      if (ndims == 1)
         elem_type = type_elem(base_type);
      else
         elem_type = sem_array_aggregate_type(composite_type, 1);

      type_t index_type = sem_index_type(composite_type, 0);

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

            if (!type_eq(field_type, tree_type(value)))
               sem_error(a, "type of value %s does not match type "
                         "of field %s %s",
                         sem_type_str(tree_type(value)),
                         istr(tree_ident(field)),
                         sem_type_str(field_type));

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

   tree_set_type(t, composite_type);
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

   type_t rec_type = tree_type(rec);
   if (type_kind(rec_type) == T_ACCESS) {
      // Record fields can be dereferenced implicitly
      value = tree_new(T_ALL);
      tree_set_value(value, make_ref(rec));
      tree_set_type(value, type_access(tree_type(rec)));
   }
   else
      assert(type_is_record(rec_type));

   if (tree_kind(rec) == T_FIELD_DECL) {
      value = tree_new(T_REF);
      tree_set_loc(value, tree_loc(t));
      tree_set_ident(value, base);
      sem_convert_to_record_ref(value, rec);
   }
   else if (value == NULL)
      value = make_ref(rec);

   tree_change_kind(t, T_RECORD_REF);
   tree_set_value(t, value);
   tree_set_ident(t, tree_ident(decl));
   tree_set_type(t, tree_type(decl));
}

static bool sem_check_ref(tree_t t)
{
   tree_t decl = NULL, next;
   ident_t name = tree_ident(t);
   int n = 0;
   do {
      if ((next = scope_find_nth(name, n))) {
         tree_kind_t kind = tree_kind(next);
         if ((kind == T_ENTITY) || (kind == T_ARCH) || (kind == T_COMPONENT))
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
         sem_error(t, "undefined identifier %s", istr(name));
      else if (n == 1)
         sem_error(t, "name %s cannot be used in this context%s",
                   istr(name), type_set_fmt());
      else
         sem_error(t, "no suitable overload for identifier %s in "
                   "context%s", istr(name), type_set_fmt());
   }

   switch (tree_kind(decl)) {
   case T_VAR_DECL:
   case T_SIGNAL_DECL:
   case T_PORT_DECL:
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
      break;

   case T_FIELD_DECL:
      sem_convert_to_record_ref(t, decl);
      return true;

   default:
      sem_error(t, "invalid use of %s", istr(tree_ident(t)));
   }

   tree_set_ref(t, decl);

   return true;
}

static bool sem_check_record_ref(tree_t t)
{
   tree_t value = tree_value(t);
   if (!sem_check_constrained(value, NULL))
      return false;

   type_t value_type = tree_type(value);
   if (type_kind(value_type) != T_RECORD)
      sem_error(value, "expected record type but found %s",
                sem_type_str(value_type));

   ident_t fname = tree_ident(t);

   const int nfields = type_fields(value_type);
   tree_t field;
   for (int i = 0; i < nfields; i++, field = NULL) {
      field = type_field(value_type, i);
      if (tree_ident(field) == fname)
         break;
   }

   if (field == NULL)
      sem_error(t, "record type %s has no field %s",
                sem_type_str(value_type), istr(fname));

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
      sem_error(t, "invalid array reference");

   const int nindex  = sem_array_dimension(type);
   const int nparams = tree_params(t);

   if (nparams != nindex)
      sem_error(t, "array %s has %d dimensions but %d indices given",
                istr(tree_ident(value)), nindex, nparams);

   bool ok = true;
   for (int i = 0; i < nparams; i++) {
      tree_t p = tree_param(t, i);
      if (tree_subkind(p) != P_POS)
         sem_error(t, "only scalar references supported");

      type_t expect = sem_index_type(type, i);

      tree_t value = tree_value(p);

      if (tree_kind(value) == T_REF) {
         // Handle slices using a subtype as a range
         tree_t decl = scope_find(tree_ident(value));
         if (tree_kind(decl) == T_TYPE_DECL) {
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
   if (!sem_check_range(&r, sem_index_type(array_type, 0)))
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
      else if ((kind == T_ARCH) || (kind == T_ENTITY)
               || (kind == T_COMPONENT) || (kind == T_FUNC_DECL)
               || (kind == T_FUNC_BODY) || (kind == T_PROC_DECL)
               || (kind == T_PROC_BODY)) {
         // Special case for attributes of entities and architectures
         tree_set_ref(name, decl);

         special = true;
      }
   }

   if (!special && !sem_check_constrained(name, NULL))
      return false;

   if (tree_has_type(name) && (type_kind(tree_type(name)) == T_ACCESS)) {
      // Convert implicit dereference such as PTR'X to PTR.ALL'X
      sem_implicit_dereference(t, tree_name, tree_set_name);
      name = tree_name(t);
   }

   bool allow_user = true;
   switch (tree_kind(name)) {
   case T_REF:
      decl = tree_ref(name);
      break;

   case T_ALL:
      decl = tree_ref(tree_value(name));
      break;

   default:
      if (sem_static_name(name)) {
         while (tree_kind((name = tree_value(name))) != T_REF)
            ;
         decl = tree_ref(name);
         allow_user = false;   // LRM disallows user-defined attributes
                               // where prefix is slice of sub-element
      }
      else
         sem_error(t, "invalid attribute reference");
   }

   ident_t attr = tree_ident(t);

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
      tree_set_type(t, type_result(ftype));

      char *buf = xasprintf("_arg%d", tree_ports(a) - 1);
      ident_t pname = ident_new(buf);
      free(buf);

      // For an expression X'A(..) add X as a final parameter
      bool already_added = false;
      for (int i = 0; (i < tree_params(t)) && !already_added; i++) {
         tree_t p = tree_param(t, i);
         if ((tree_subkind(p) == P_NAMED)
             && (tree_ident(tree_name(p)) == pname))
            already_added = true;
      }

      if (!already_added) {
         tree_t p = tree_new(T_PARAM);
         tree_set_loc(p, tree_loc(name));
         tree_set_subkind(p, P_NAMED);
         tree_set_value(p, name);
         tree_set_name(p, make_ref(tree_port(a, tree_ports(a) - 1)));

         tree_add_param(t, p);
      }

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

      tree_set_ref(t, a);
   }
   else if (!allow_user)
      sem_error(t, "prefix of user defined attribute reference cannot "
                "denote a sub-element or slice of an object");
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

static bool sem_check_map(tree_t t, tree_t unit,
                          tree_formals_t tree_Fs, tree_formal_t tree_F,
                          tree_actuals_t tree_As, tree_actual_t tree_A)
{
   // Check there is an actual for each formal port or generic
   // Rules for maps are described in LRM 93 section 5.2.1.2

   const int nformals = tree_Fs(unit);
   const int nactuals = tree_As(t);

   bool ok = true;

   struct {
      tree_t decl;
      bool   have;
      bool   partial;
   } formals[nformals];

   for (int i = 0; i < nformals; i++) {
      formals[i].decl    = tree_F(unit, i);
      formals[i].have    = false;
      formals[i].partial = false;
   }

   for (int i = 0; i < nactuals; i++) {
      tree_t p = tree_A(t, i);
      tree_t value = tree_value(p);
      tree_t decl = NULL;
      type_t type = NULL;

      switch (tree_subkind(p)) {
      case P_POS:
         {
            const int pos = tree_pos(p);
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
            tree_t name = tree_name(p);

            tree_t ref = NULL;
            switch (tree_kind(name)) {
            case T_REF:
               ref = name;
               break;

            case T_ARRAY_SLICE:
               {
                  range_t r = tree_range(name);
                  if (!sem_check_range(&r, sem_std_type("INTEGER")))
                     return false;
                  tree_set_range(name, r);

                  ref = tree_value(name);
               }
               break;

            case T_FCALL:
               {
                  tree_t ref = tree_new(T_REF);
                  tree_set_ident(ref, tree_ident(name));

                  tree_change_kind(name, T_ARRAY_REF);
                  tree_set_value(name, ref);
               }
               // Fall-through

            case T_ARRAY_REF:
               {
                  const int nparams = tree_params(name);
                  for (int i = 0; i < nparams; i++) {
                     tree_t value = tree_value(tree_param(name, i));
                     if (!sem_check(value))
                        return false;
                  }

                  ref = tree_value(name);
               }
               break;

            default:
               assert(false);
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

            type = tree_type(decl);
            if (tree_kind(name) == T_ARRAY_REF)
               type = type_elem(type);

            break;
         }
      }

      assert(type != NULL);

      ok = sem_check_constrained(value, type) && ok;

      if (!ok)
         continue;

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
   }

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

static bool sem_check_instance(tree_t t)
{
   tree_t unit = NULL;
   ident_t name = tree_ident2(t);

   switch (tree_class(t)) {
   case C_ENTITY:
      {
         // Find the referenced design unit
         ident_t prefix = ident_until(name, '-');
         unit = lib_get_check_stale(lib_work(), prefix);
         if (unit == NULL)
            sem_error(t, "cannot find unit %s", istr(prefix));

         if (tree_kind(unit) != T_ENTITY)
            sem_error(t, "unit %s is not an entity", istr(prefix));
      }
      break;

   case C_COMPONENT:
      {
         // Find the component declaration
         unit = scope_find(name);
         if (unit == NULL)
            sem_error(t, "no declaration for component %s", istr(name));

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

   // A constant reference with a locally static value
   if ((kind == T_REF) && (tree_kind(tree_ref(t)) == T_CONST_DECL))
      return sem_locally_static(tree_value(tree_ref(t)));

   // An alias of a locally static name
   if (kind == T_ALIAS)
      return sem_locally_static(tree_value(t));

   // A function call of an implicit operator with locally static actuals
   if (kind == T_FCALL) {
      tree_t decl = tree_ref(t);
      if (tree_attr_str(decl, builtin_i) == NULL)
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

         // Check for locally static subtype
         type_t type = tree_type(tree_ref(tree_name(t)));

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
      range_t r = type_dim(type, 0);
      if (r.kind != RANGE_TO && r.kind != RANGE_DOWNTO)
         return false;

      if (!sem_locally_static(r.left) || !sem_locally_static(r.right))
         return false;

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
            return true;
         case T_ALIAS:
            return sem_static_name(tree_value(decl));
         default:
            return false;
         }
      }

   case T_RECORD_REF:
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

   if (kind == T_LITERAL) {
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

   bool ok = true;

   const int ndecls = tree_decls(t);
   for (int i = 0; i < ndecls; i++)
      ok = sem_check(tree_decl(t, i)) && ok;

   ok = ok && sem_check_stmts(t, tree_stmt, tree_stmts(t));

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
      struct loop_stack *it;
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
   if (tree_class(t) == C_LABEL)
      return true;

   tree_t obj_decl = scope_find(tree_ident2(t));
   if (obj_decl == NULL)
      sem_error(t, "undefined identifier %s", istr(tree_ident2(t)));

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

         tree_set_value(t, sem_default_value(type));
      }
      break;

   case T_REF:
      {
         type = type_new(T_UNRESOLVED);
         type_set_ident(type, tree_ident(value));
         if (!sem_check_type(value, &type))
            return false;

         tree_set_value(t, sem_default_value(type));
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

static void sem_intern_strings(void)
{
   // Intern some commonly used strings

   builtin_i        = ident_new("builtin");
   std_standard_i   = ident_new("STD.STANDARD");
   formal_i         = ident_new("formal");
   locally_static_i = ident_new("locally_static");
   elab_copy_i      = ident_new("elab_copy");
}

bool sem_check(tree_t t)
{
   static bool have_interned = false;
   if (!have_interned) {
      sem_intern_strings();
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
   case T_TYPE_CONV:
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
   case T_CONTEXT:
      return sem_check_use_clause(t);
   case T_TYPE_CONV:
      return sem_check_conversion(t);
   default:
      sem_error(t, "cannot check %s", tree_kind_str(tree_kind(t)));
   }
}

int sem_errors(void)
{
   return errors;
}

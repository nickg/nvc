//
//  Copyright (C) 2011-2013  Nick Gasson
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

#include <assert.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>
#include <ctype.h>
#include <errno.h>
#include <sys/stat.h>
#include <sys/types.h>

struct ident_list {
   ident_t           ident;
   struct ident_list *next;
};

struct btree {
   tree_t       tree;
   ident_t      name;
   struct btree *left;
   struct btree *right;
};

struct scope {
   struct scope      *down;

   struct btree       *decls;
   tree_t             subprog;

   // For design unit scopes
   ident_t            prefix;
   struct ident_list *imported;
   bool               is_package;
};

struct loop_stack {
   struct loop_stack *up;
   ident_t            name;
};

#define MAX_OVERLOADS 32

struct type_set {
   type_t   *members;
   unsigned  n_members;
   unsigned  alloc;

   struct type_set *down;
};

static bool sem_check_constrained(tree_t t, type_t type);
static bool sem_check_array_ref(tree_t t);
static bool sem_declare(tree_t decl);
static bool sem_locally_static(tree_t t);
static tree_t sem_check_lvalue(tree_t t);
static bool sem_check_type(tree_t t, type_t *ptype);

static struct scope      *top_scope = NULL;
static int                errors = 0;
static struct type_set   *top_type_set = NULL;
static struct loop_stack *loop_stack = NULL;
static ident_t            builtin_i;
static ident_t            std_standard_i;

#define sem_error(t, ...) do {                        \
      error_at(t ? tree_loc(t) : NULL , __VA_ARGS__); \
      errors++;                                       \
      return false;                                   \
   } while (0)

static void scope_push(ident_t prefix)
{
   struct scope *s = xmalloc(sizeof(struct scope));
   s->decls      = NULL;
   s->prefix     = prefix;
   s->imported   = NULL;
   s->down       = top_scope;
   s->subprog    = (top_scope ? top_scope->subprog : NULL) ;
   s->is_package = false;

   top_scope = s;
}

static void scope_btree_free(struct btree *b)
{
   if (b != NULL) {
      scope_btree_free(b->left);
      scope_btree_free(b->right);
      free(b);
   }
}

static void scope_ident_list_free(struct ident_list *list)
{
   struct ident_list *it = list;
   while (it != NULL) {
      struct ident_list *next = it->next;
      free(it);
      it = next;
   }
}

static void scope_pop(void)
{
   assert(top_scope != NULL);

   scope_ident_list_free(top_scope->imported);
   scope_btree_free(top_scope->decls);

   struct scope *s = top_scope;
   top_scope = s->down;
   free(s);
}

static void scope_ident_list_add(struct ident_list **list, ident_t i)
{
   struct ident_list *c = xmalloc(sizeof(struct ident_list));
   c->ident = i;
   c->next  = *list;

   *list = c;
}

static void scope_apply_prefix(tree_t t)
{
   if (top_scope->prefix)
      tree_set_ident(t, ident_prefix(top_scope->prefix,
                                     tree_ident(t), '.'));
}

#if 0
static void scope_dump_aux(struct btree *b)
{
   printf("%-30s%s\n", istr(b->name), type_pp(tree_type(b->tree)));
   if (b->left)
      scope_dump_aux(b->left);
   if (b->right)
      scope_dump_aux(b->right);
}

static void scope_dump(void)
{
   struct scope *s = top_scope;

   while (s != NULL) {
      printf("---------------------------\n");
      if (s->decls)
         scope_dump_aux(s->decls);
      s = s->down;
   }
}
#endif

static tree_t scope_find_in(ident_t i, struct scope *s, bool recur, int k)
{
   if (s == NULL)
      return NULL;
   else {
      struct btree *search = s->decls;

      while (search != NULL) {
         if (search->name == i) {
            if (k == 0)
               return search->tree;
            else
               --k;
         }

         search = ((i < search->name) ? search->left : search->right);
      }

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

static bool scope_hides(tree_t a, tree_t b)
{
   // True if declaration of b hides a
   if ((tree_kind(a) == T_COMPONENT) || (tree_kind(b) == T_COMPONENT))
      return false;
   else if (type_eq(tree_type(a), tree_type(b))) {
      return (tree_attr_str(a, builtin_i) != NULL)
         && (tree_attr_str(b, builtin_i) == NULL);
   }
   else
      return false;
}

static struct btree *scope_btree_new(tree_t t, ident_t name)
{
   struct btree *b = xmalloc(sizeof(struct btree));
   b->tree  = t;
   b->name  = name;
   b->left  = NULL;
   b->right = NULL;

   return b;
}

static void scope_insert_at(tree_t t, ident_t name, struct btree *where)
{
   if (where == NULL)
      top_scope->decls = scope_btree_new(t, name);
   else if (scope_hides(where->tree, t))
      where->tree = t;
   else {
      struct btree **nextp =
         ((name < where->name) ? &where->left : &where->right);

      if (*nextp == NULL)
         *nextp = scope_btree_new(t, name);
      else
         scope_insert_at(t, name, *nextp);
   }
}

static void scope_replace_at(tree_t t, tree_t with, struct btree *where)
{
   assert(where != NULL);
   if (where->tree == t)
      where->tree = with;

   // We need to walk over the whole tree as this may appear under
   // multiple names

   if (where->left != NULL)
      scope_replace_at(t, with, where->left);
   if (where->right != NULL)
      scope_replace_at(t, with, where->right);
}

static bool scope_insert(tree_t t)
{
   assert(top_scope != NULL);

   if (!scope_can_overload(t)
       && scope_find_in(tree_ident(t), top_scope, false, 0))
      sem_error(t, "%s already declared in this scope",
                istr(tree_ident(t)));

   scope_insert_at(t, tree_ident(t), top_scope->decls);
   return true;
}

static void scope_insert_alias(tree_t t, ident_t name)
{
   assert(top_scope != NULL);
   scope_insert_at(t, name, top_scope->decls);
}

static void scope_replace(tree_t t, tree_t with)
{
   assert(top_scope != NULL);

   scope_replace_at(t, with, top_scope->decls);
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

static bool sem_check_stale(lib_t lib, tree_t t)
{
   // Check if the source file corresponding to t has been modified
   // more recently than the library unit

   const loc_t *l = tree_loc(t);
   if (l->file == NULL)
      return true;

   struct stat st;
   if (stat(l->file, &st) < 0) {
      if (errno != ENOENT)
         fatal_errno("%s", l->file);
      else
         return true;
   }

   if (st.st_mtime > lib_mtime(lib, tree_ident(t)))
      sem_error(NULL, "source file %s for unit %s has changed and must "
                "be reanalysed", l->file, istr(tree_ident(t)));
   else
      return true;
}

static bool scope_import_unit(context_t ctx, lib_t lib, bool all)
{
   // Check we haven't already imported this
   for (struct scope *s = top_scope; s != NULL; s = s->down) {
      struct ident_list *it;
      for (it = s->imported; it != NULL; it = it->next) {
         if (it->ident == ctx.name)
            return true;
      }
   }

   tree_t unit = lib_get(lib, ctx.name);
   if (unit == NULL) {
      error_at(&ctx.loc, "unit %s not found in library %s",
               istr(ctx.name), istr(lib_name(lib)));
      errors++;
      return false;
   }

   if (!sem_check_stale(lib, unit))
      return false;

   for (unsigned n = 0; n < tree_decls(unit); n++) {
      tree_t decl = tree_decl(unit, n);
      if (tree_kind(decl) == T_ATTR_SPEC)
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

   scope_ident_list_add(&top_scope->imported, ctx.name);
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

static void type_set_force(type_t t)
{
   assert(top_type_set != NULL);
   assert(t != NULL);
   assert(type_kind(t) != T_UNRESOLVED);

   top_type_set->members[0] = t;
   top_type_set->n_members  = 1;
}

static bool type_set_restrict(bool (*pred)(type_t))
{
   assert(top_type_set != NULL);

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

static tree_t sem_make_int(int i)
{
   literal_t l;
   l.kind = L_INT;
   l.i    = i;

   tree_t t = tree_new(T_LITERAL);
   tree_set_literal(t, l);
   tree_set_type(t, sem_std_type("INTEGER"));

   return t;
}

static tree_t sem_make_ref(tree_t to)
{
   tree_t t = tree_new(T_REF);
   tree_set_ident(t, tree_ident(to));
   tree_set_ref(t, to);
   tree_set_type(t, tree_type(to));
   return t;
}

static void sem_add_port(tree_t d, type_t type, port_mode_t mode, tree_t def)
{
   type_t ftype = tree_type(d);

   char argname[16];
   snprintf(argname, sizeof(argname), "_arg%d", type_params(ftype));

   tree_t port = tree_new(T_PORT_DECL);
   tree_set_ident(port, ident_new(argname));
   tree_set_type(port, type);
   tree_set_port_mode(port, mode);
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
   return sem_make_ref(lit);
}

static tree_t sem_int_lit(type_t type, int64_t i)
{
   literal_t l;
   l.kind = L_INT;
   l.i = i;

   tree_t f = tree_new(T_LITERAL);
   tree_set_literal(f, l);
   tree_set_type(f, type);

   return f;
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
      sem_declare_binary(ident_new("\"<\""), t, t, std_bool, "alt");
      sem_declare_binary(ident_new("\"<=\""), t, t, std_bool, "aleq");
      sem_declare_binary(ident_new("\">\""), t, t, std_bool, "agt");
      sem_declare_binary(ident_new("\">=\""), t, t, std_bool, "ageq");
      break;

   case T_RECORD:
      // Operators on records
      sem_declare_binary(ident_new("\"=\""), t, t, std_bool, "req");
      sem_declare_binary(ident_new("\"/=\""), t, t, std_bool, "rneq");
      break;

   case T_PHYSICAL:
      // Multiplication
      sem_declare_binary(mult, t, std_int, t, "mul");
      //sem_declare_binary(mult, t, std_real, t, "mul");
      sem_declare_binary(mult, std_int, t, t, "mul");
      //sem_declare_binary(mult, std_real, t, t, "mul");

      // Division
      sem_declare_binary(div, t, std_int, t, "div");
      //sem_declare_binary(div, t, std_real, t, "div");
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
         sem_add_port(file_open1, open_kind,
                      PORT_IN, sem_make_ref(read_mode));
         scope_insert(file_open1);

         tree_t file_open2 = sem_builtin_proc(file_open_i, "file_open2");
         sem_add_port(file_open2, open_status, PORT_OUT, NULL);
         sem_add_port(file_open2, t, PORT_INOUT, NULL);
         sem_add_port(file_open2, std_string, PORT_IN, NULL);
         sem_add_port(file_open2, open_kind,
                      PORT_IN, sem_make_ref(read_mode));
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
   case T_INTEGER:
   case T_REAL:
   case T_PHYSICAL:
   case T_SUBTYPE:
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

         tree_t image = sem_builtin_fn(ident_new("NVC.BUILTIN.IMAGE"),
                                       std_string, "image", t, NULL);
         tree_add_attr_tree(decl, ident_new("IMAGE"), image);
      }
      break;

   case T_ENUM:
      {
         tree_t left  = type_enum_literal(t, 0);
         tree_t right = type_enum_literal(t, type_enum_literals(t) - 1);
         tree_add_attr_tree(decl, ident_new("LEFT"), sem_make_ref(left));
         tree_add_attr_tree(decl, ident_new("RIGHT"), sem_make_ref(right));
         tree_add_attr_tree(decl, ident_new("LOW"), sem_make_ref(left));
         tree_add_attr_tree(decl, ident_new("HIGH"), sem_make_ref(right));

         tree_t image = sem_builtin_fn(ident_new("NVC.BUILTIN.IMAGE"),
                                       std_string, "image", t, NULL);
         tree_add_attr_tree(decl, ident_new("IMAGE"), image);
      }
      break;

   default:
      break;
   }

   if (type_is_array(t)) {
      ident_t length_i = ident_new("LENGTH");
      tree_add_attr_tree(decl, length_i,
                         sem_builtin_fn(length_i, std_int, "length", t, NULL));
   }

   switch (type_kind(t)) {
   case T_INTEGER:
   case T_REAL:
   case T_PHYSICAL:
   case T_SUBTYPE:
   case T_ENUM:
      {
         tree_t succ = sem_builtin_fn(ident_new("NVC.BUILTIN.SUCC"),
                                      t, "succ", t, NULL);
         tree_add_attr_tree(decl, ident_new("SUCC"), succ);

         tree_t pred = sem_builtin_fn(ident_new("NVC.BUILTIN.PRED"),
                                      t, "pred", t, NULL);
         tree_add_attr_tree(decl, ident_new("PRED"), pred);

         tree_t leftof = sem_builtin_fn(ident_new("NVC.BUILTIN.LEFTOF"),
                                        t, "leftof", t, NULL);
         tree_add_attr_tree(decl, ident_new("LEFTOF"), leftof);

         tree_t rightof = sem_builtin_fn(ident_new("NVC.BUILTIN.RIGHTOF"),
                                         t, "rightof", t, NULL);
         tree_add_attr_tree(decl, ident_new("RIGHTOF"), rightof);
      }
      break;

   default:
      break;
   }
}

static bool sem_check_subtype(tree_t t, type_t type, type_t *pbase)
{
   // Resolve a subtype to its base type

   while (type_kind(type) == T_SUBTYPE) {
      type_t base = type_base(type);
      if (type_kind(base) == T_UNRESOLVED) {
         tree_t base_decl = scope_find(type_ident(base));
         if (base_decl == NULL)
            sem_error(t, "type %s is not defined", sem_type_str(base));

         base = tree_type(base_decl);
         type_set_base(type, base);
      }

      // If the subtype is not constrained then give it the same
      // range as its base type
      if (type_dims(type) == 0) {
         switch (type_kind(base)) {
         case T_ENUM:
            {
               range_t r = {
                  .kind  = RANGE_TO,
                  .left  = sem_make_int(0),
                  .right = sem_make_int(type_enum_literals(base) - 1)
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

         default:
            sem_error(t, "sorry, this form of subtype is not supported");
         }
      }

      type = base;
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

   // Declare any predefined operators and attributes
   if (tree_kind(decl) == T_TYPE_DECL)
      sem_declare_predefined_ops(decl);

   // No futher processing needed for subtypes
   if (type_k == T_SUBTYPE)
      return true;

   bool ok = true;

   type_t type = tree_type(decl);
   switch (type_kind(type)) {
   case T_ENUM:
      // Need to add each literal to the scope
      for (unsigned i = 0; i < type_enum_literals(type); i++)
         ok = ok && scope_insert(type_enum_literal(type, i));
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
      assert(r->right == NULL);
      assert(tree_kind(r->left) == T_ATTR_REF
             || tree_kind(r->left) == T_REF);

      tree_t decl = scope_find(tree_ident(r->left));
      if (decl == NULL)
         sem_error(r->left, "undefined identifier %s",
                   istr(tree_ident(r->left)));

      type_t type = tree_type(decl);
      type_kind_t kind = type_kind(type);
      switch (kind) {
      case T_CARRAY:
         *r = type_dim(type, 0);
         return true;
      case T_ENUM:
      case T_UARRAY:
      case T_SUBTYPE:
         {
            tree_t a = tree_new(T_ATTR_REF);
            tree_set_ident(a, tree_ident(r->left));
            tree_set_ident2(a, ident_new("LEFT"));

            tree_t b = tree_new(T_ATTR_REF);
            tree_set_ident(b, tree_ident(r->left));
            tree_set_ident2(b, ident_new("RIGHT"));

            // If this is an unconstrained array then we can
            // only find out the direction at runtime
            r->kind  = (kind == T_UARRAY ? RANGE_DYN : RANGE_TO);
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

   if (!type_eq(tree_type(r->left), tree_type(r->right)))
      sem_error(r->right, "type mismatch in range");

   return true;
}

static bool sem_check_context(tree_t t)
{
   // The std.standard package is also implicit unless we are
   // bootstrapping
   if (!opt_get_int("bootstrap")) {
      lib_t std = lib_find("std", true, true);
      if (std == NULL)
         fatal("failed to find std library");

      context_t c = {
         .name = std_standard_i,
         .loc  = LOC_INVALID,
         .all  = true
      };
      if (!scope_import_unit(c, std, true))
         return false;
   }

   bool ok = true;
   for (unsigned n = 0; n < tree_contexts(t); n++) {
      context_t c = tree_context(t, n);

      lib_t lib = lib_find(istr(ident_until(c.name, '.')), true, true);
      if (lib != NULL)
         ok = scope_import_unit(c, lib, c.all) && ok;
      else
         ok = false;
   }

   return ok;
}

static bool sem_check_constrained(tree_t t, type_t type)
{
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
             && tree_port_mode(decl) == PORT_OUT)
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
   for (unsigned i = 0; i < type_dims(type); i++) {
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

         switch (type_kind(base)) {
         case T_UARRAY:
            {
               // Create a new constrained array type for this instance

               if (type_dims(*ptype) != type_index_constrs(base))
                  sem_error(t, "expected %d array dimensions but %d given",
                            type_index_constrs(base), type_dims(*ptype));

               if (!sem_check_array_dims(*ptype, base))
                  return false;

               type_t collapse = type_new(T_CARRAY);
               type_set_ident(collapse, type_ident(base));
               type_set_elem(collapse, type_elem(base));  // Element type

               for (unsigned i = 0; i < type_dims(*ptype); i++)
                  type_add_dim(collapse, type_dim(*ptype, i));

               *ptype = collapse;
            }
            break;

         default:
            break;
         }

         return true;
      }

   case T_UNRESOLVED:
      {
         tree_t type_decl = scope_find(type_ident(*ptype));
         if (type_decl == NULL)
            sem_error(t, "type %s is not defined", sem_type_str(*ptype));

         *ptype = tree_type(type_decl);
      }
      return true;

   default:
      assert(false);
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
         assert(ndims > 0);
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

   type_t type = tree_type(decl);
   type_kind_t kind = type_kind(type);

   if (kind == T_UARRAY) {
      const char *funs[] = { "LOW", "HIGH", "LEFT", "RIGHT", NULL };
      const char *impl[] = { "uarray_low", "uarray_high", "uarray_left",
                             "uarray_right", NULL };
      const char **f, **imp;
      for (f = funs, imp = impl; *f != NULL; f++, imp++) {
         ident_t id = ident_new(*f);
         tree_add_attr_tree(decl, id,
                            sem_builtin_fn(id, type_index_constr(type, 0),
                                           *imp, type, NULL));
      }

      ident_t asc_i = ident_new("ASCENDING");
      tree_add_attr_tree(decl, asc_i,
                         sem_builtin_fn(asc_i, std_bool,
                                        "uarray_asc", type, NULL));
   }
   else if (type_is_array(type)) {
      range_t r = type_dim(type, 0);

      tree_add_attr_tree(decl, ident_new("LEFT"), r.left);
      tree_add_attr_tree(decl, ident_new("RIGHT"), r.right);

      if (r.kind != RANGE_DYN)
         tree_add_attr_tree(decl, ident_new("ASCENDING"),
                            sem_bool_lit(std_bool, r.kind == RANGE_TO));
      else {
         ident_t asc_i = ident_new("ASCENDING");
         tree_add_attr_tree(decl, asc_i,
                            sem_builtin_fn(asc_i, std_bool,
                                           "uarray_asc", type, NULL));
      }

      if (r.kind == RANGE_TO) {
         tree_add_attr_tree(decl, ident_new("LOW"), r.left);
         tree_add_attr_tree(decl, ident_new("HIGH"), r.right);
      }
      else {
         tree_add_attr_tree(decl, ident_new("HIGH"), r.left);
         tree_add_attr_tree(decl, ident_new("LOW"), r.right);
      }
   }

   if (type_is_array(type)) {
      ident_t length_i = ident_new("LENGTH");
      tree_add_attr_tree(decl, length_i,
                         sem_builtin_fn(length_i,
                                        sem_std_type("INTEGER"),
                                        "length", type, NULL));
   }

   if ((tree_kind(decl) == T_PORT_DECL && tree_class(decl) == C_SIGNAL)
       || (tree_kind(decl) == T_SIGNAL_DECL)) {
      type_t std_string = sem_std_type("STRING");
      type_t std_time   = sem_std_type("TIME");

      ident_t event_i      = ident_new("EVENT");
      ident_t last_value_i = ident_new("LAST_VALUE");
      ident_t active_i     = ident_new("ACTIVE");
      ident_t inst_name_i  = ident_new("INSTANCE_NAME");
      ident_t path_name_i  = ident_new("PATH_NAME");
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
      tree_add_attr_tree(decl, inst_name_i,
                         sem_builtin_fn(inst_name_i, std_string,
                                        "instance_name", type, NULL));
      tree_add_attr_tree(decl, path_name_i,
                         sem_builtin_fn(path_name_i, std_string,
                                        "path_name", type, NULL));
      tree_add_attr_tree(decl, last_event_i,
                         sem_builtin_fn(last_event_i, std_time,
                                        "last_event", type, NULL));
   }
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
         for (int i = type_dims(type) - 1 ; i >= 0; i--) {
            tree_t val = (def ? def : sem_default_value(type_elem(base)));
            def = tree_new(T_AGGREGATE);
            assoc_t a = {
               .kind  = A_OTHERS,
               .value = val
            };
            tree_add_assoc(def, a);
         }
         return def;
      }

   case T_INTEGER:
   case T_PHYSICAL:
   case T_REAL:
      return type_dim(type, 0).left;

   case T_ENUM:
      return sem_make_ref(type_enum_literal(base, 0));

   case T_RECORD:
      {
         tree_t def = tree_new(T_AGGREGATE);
         const int nfields = type_fields(base);
         for (int i = 0; i < nfields; i++) {
            tree_t field = type_field(base, i);
            assoc_t a = {
               .kind  = A_POS,
               .value = sem_default_value(tree_type(field))
            };
            tree_add_assoc(def, a);
         }
         return def;
      }

   case T_ACCESS:
      {
         tree_t null = tree_new(T_LITERAL);
         literal_t l = { .kind = L_NULL };
         tree_set_literal(null, l);
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
      if (type_kind(field_type) == T_RECORD)
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

   if ((type_kind(type) == T_UARRAY) && (kind != T_CONST_DECL))
      sem_error(t, "type %s is unconstrained", sem_type_str(type));

   if (!tree_has_value(t) && (kind != T_PORT_DECL) && (kind != T_CONST_DECL))
      tree_set_value(t, sem_default_value(type));

   if (tree_has_value(t)) {
      tree_t value = tree_value(t);
      if (!sem_check_constrained(value, type))
         return false;

      if (!type_eq(type, tree_type(value)))
         sem_error(value, "type of initial value %s does not match type "
                   "of declaration %s", sem_type_str(tree_type(value)),
                   sem_type_str(type));
   }

   if (kind == T_PORT_DECL && tree_class(t) == C_DEFAULT)
      tree_set_class(t, C_SIGNAL);

   if (type_kind(type) == T_RECORD) {
      if ((kind == T_PORT_DECL) || (kind == T_SIGNAL_DECL))
         sem_error(t, "sorry, records are not yet allowed as signals");

      sem_declare_fields(type, tree_ident(t));
   }
   else if (type_kind(type) == T_ACCESS) {
      type_t deref_type = type_access(type);
      if (type_kind(deref_type) == T_RECORD) {
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

   if (type_kind(type) == T_RECORD)
      sem_declare_fields(type, tree_ident(t));

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
   if (!sem_check(tree_value(t)))
      return false;

   if (tree_has_type(t)) {
      // TODO: this is not correct - check LRM
      type_t type = tree_type(t);
      if (!sem_check_type(t, &type))
         return false;
      tree_set_type(t, type);
   }
   else
      tree_set_type(t, tree_type(tree_value(t)));

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
      if (tree_port_mode(p) != PORT_IN)
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

static bool sem_check_func_decl(tree_t t)
{
   if (!sem_check_func_ports(t))
      return false;

   if (sem_check_duplicate(t, T_FUNC_DECL))
      sem_error(t, "duplicate declaration of function %s",
                istr(tree_ident(t)));

   scope_apply_prefix(t);
   return scope_insert(t);
}

static bool sem_check_func_body(tree_t t)
{
   if (!sem_check_func_ports(t))
      return false;

   ident_t unqual = top_scope->prefix ? tree_ident(t) : NULL;
   scope_apply_prefix(t);

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

   for (unsigned i = 0; i < tree_ports(t); i++) {
      tree_t p = tree_port(t, i);
      sem_add_attributes(p);
      ok = scope_insert(p) && ok;
   }

   for (unsigned i = 0; i < tree_decls(t); i++)
      ok = sem_check(tree_decl(t, i)) && ok;

   if (ok) {
      for (unsigned i = 0; i < tree_stmts(t); i++)
         ok = sem_check(tree_stmt(t, i)) && ok;
   }

   scope_pop();

   unsigned nret = tree_visit_only(t, NULL, NULL, T_RETURN);
   if (nret == 0)
      sem_error(t, "function must contain a return statement");

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
         switch (tree_port_mode(p)) {
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

   scope_apply_prefix(t);
   return scope_insert(t);
}

static bool sem_check_proc_body(tree_t t)
{
   if (!sem_check_proc_ports(t))
      return false;

   scope_apply_prefix(t);

   // If there is no declaration for this procedure add to the scope
   if (!sem_check_duplicate(t, T_PROC_DECL)) {
      if (!scope_insert(t))
         return false;
   }

   scope_push(NULL);
   top_scope->subprog = t;

   bool ok = true;

   for (unsigned i = 0; i < tree_ports(t); i++) {
      tree_t p = tree_port(t, i);
      sem_add_attributes(p);
      ok = scope_insert(p) && ok;
   }

   for (unsigned i = 0; i < tree_decls(t); i++)
      ok = sem_check(tree_decl(t, i)) && ok;

   if (ok) {
      for (unsigned i = 0; i < tree_stmts(t); i++)
         ok = sem_check(tree_stmt(t, i)) && ok;
   }

   scope_pop();
   return ok;
}

static bool sem_check_sensitivity(tree_t t)
{
   bool ok = true;
   for (unsigned i = 0; i < tree_triggers(t); i++) {
      tree_t r = tree_trigger(t, i);
      ok = sem_check(r) && sem_readable(r) && ok;

      if (ok) {
         // Can only reference signals in sensitivity list
         tree_t decl = sem_check_lvalue(r);
         switch (tree_kind(decl)) {
         case T_SIGNAL_DECL:
         case T_PORT_DECL:
            break;
         default:
            sem_error(r, "name %s in sensitivity list is not a signal",
                      istr(tree_ident(decl)));
         }
      }
   }

   return ok;
}

static bool sem_check_process(tree_t t)
{
   scope_push(NULL);

   bool ok = sem_check_sensitivity(t);

   for (unsigned n = 0; n < tree_decls(t); n++)
      ok = sem_check(tree_decl(t, n)) && ok;

   if (ok) {
      for (unsigned n = 0; n < tree_stmts(t); n++)
         ok = sem_check(tree_stmt(t, n)) && ok;
   }

   scope_pop();

   if (tree_triggers(t) > 0) {
      // No wait statements allowed in process with sensitivity list
      if (tree_visit_only(t, NULL, NULL, T_WAIT) > 0)
         sem_error(t, "wait statement not allowed in process "
                   "with sensitvity list");
   }

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

         if (sem_check(decl) && (tree_kind(decl) != T_ATTR_SPEC)) {
            // Make the unqualified name visible inside the package
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
   context_t c = {
      .name = qual,
      .loc  = *tree_loc(t),
      .all  = true
   };
   ok = ok && scope_import_unit(c, lib_work(), true);

   tree_t pack = NULL;
   if (ok) {
      pack = lib_get(lib_work(), c.name);
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

   for (unsigned n = 0; n < tree_generics(t); n++) {
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

      ok = sem_check(g) && ok;
   }

   if (ok) {
      // Make generics visible in this region
      for (unsigned n = 0; n < tree_generics(t); n++)
         ok = scope_insert(tree_generic(t, n)) && ok;
   }

   return ok;
}

static bool sem_check_ports(tree_t t)
{
   bool ok = true;

   for (unsigned n = 0; n < tree_ports(t); n++) {
      tree_t p = tree_port(t, n);

      if (tree_class(p) == C_DEFAULT)
         tree_set_class(p, C_SIGNAL);

      ok = sem_check(p) && ok;
   }

   return ok;
}

static bool sem_check_component(tree_t t)
{
   scope_push(NULL);

   bool ok = sem_check_generics(t) && sem_check_ports(t);

   scope_pop();

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
   tree_t e = lib_get(lib_work(),
                      ident_prefix(lib_name(lib_work()),
                                   tree_ident2(t), '.'));
   if (e == NULL)
      sem_error(t, "missing declaration for entity %s",
                istr(tree_ident2(t)));

   if (!sem_check_stale(lib_work(), e))
      return false;

   assert(top_scope == NULL);
   scope_push(NULL);

   // Make all port and generic declarations available in this scope

   bool ok = sem_check_context(e) && sem_check_context(t);

   scope_push(NULL);

   for (unsigned n = 0; n < tree_ports(e); n++)
      scope_insert(tree_port(e, n));

   for (unsigned n = 0; n < tree_generics(e); n++)
      scope_insert(tree_generic(e, n));

   // Now check the architecture itself

   for (unsigned n = 0; n < tree_decls(t); n++)
      ok = sem_check(tree_decl(t, n)) && ok;

   if (ok) {
      for (unsigned n = 0; n < tree_stmts(t); n++)
         ok = sem_check(tree_stmt(t, n)) && ok;
   }

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
      error_at(tree_loc(t), "not a suitable l-value");
      ++errors;
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
      return false;

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
   tree_t decl = sem_check_lvalue(target);
   if (decl == NULL)
      return false;

   switch (tree_kind(decl)) {
   case T_SIGNAL_DECL:
      break;

   case T_PORT_DECL:
      if (tree_port_mode(decl) == PORT_IN)
         sem_error(target, "cannot assign to input port %s",
                   istr(tree_ident(target)));
      break;

   default:
      sem_error(target, "invalid target of signal assignment");
   }

   return true;
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

   if (!sem_check(target))
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

   if (!sem_check(target))
      return false;

   type_t std_bool = sem_std_type("BOOLEAN");

   for (unsigned i = 0; i < tree_conds(t); i++) {
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
   return (type_kind(a) == T_UARRAY
           ? type_index_constrs(a)
           : type_dims(a));
}

static bool sem_check_conversion(tree_t t)
{
   // Type conversions are described in LRM 93 section 7.3.5

   if (tree_params(t) != 1)
      sem_error(t, "type conversions must have exactly one parameter");

   // Really we should push the set of types that are closely related
   // to the one being converted to
   param_t p = tree_param(t, 0);
   if (!sem_check_constrained(p.value, NULL))
      return false;

   type_t from = tree_type(p.value);
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

static bool sem_maybe_ambiguous(tree_t t)
{
   switch (tree_kind(t)) {
   case T_REF:
      {
         tree_t decl = scope_find(tree_ident(t));
         return (decl != NULL && tree_kind(decl) == T_ENUM_LIT);
      }
   case T_AGGREGATE:
      return true;
   default:
      return false;
   }
}

static type_t sem_find_param_type(param_t param, tree_t decl)
{
   type_t decl_type = tree_type(decl);

   switch (param.kind) {
   case P_POS:
      // Simple case of positional parameters is just the same index
      // into the port list
      return type_param(decl_type, param.pos);

   case P_NAMED:
      // Need to search through the port list for a matching name
      {
         const int nports = tree_ports(decl);
         for (int i = 0; i < nports; i++) {
            if (tree_ident(tree_port(decl, i)) == param.name)
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
   bool ambiguous[nparams];
   for (int i = 0; i < nparams; i++) {
      param_t p = tree_param(t, i);
      ambiguous[i] = sem_maybe_ambiguous(p.value);
   }

   // First pass: only check those parameters which are unambiguous
   for (int i = 0; i < nparams; i++) {
      if (ambiguous[i])
         continue;

      type_set_push();

      param_t p = tree_param(t, i);
      type_t param_types[n_overloads];

      for (int j = 0; j < n_overloads; j++) {
         if (overloads[j] != NULL) {
            param_types[j] = sem_find_param_type(p, overloads[j]);
            if (param_types[j] != NULL)
               type_set_add(param_types[j]);
         }
      }

      bool ok = sem_check(p.value);

      type_set_pop();

      if (ok) {
         // Delete all overloads which don't match this parameter type
         type_t ptype = tree_type(p.value);
         for (int j = 0; j < n_overloads; j++) {
            if (overloads[j] != NULL) {
               if ((param_types[j] == NULL)
                   || !type_eq(param_types[j], ptype)) {
                  overloads[j] = NULL;
               }
            }
         }
      }
      else
         return false;
   }

   // Second pass: now the set of overloads has been constrained check
   // those parameters which might be ambiguous
   for (int i = 0; i < nparams; i++) {
      if (!ambiguous[i])
         continue;

      type_set_push();

      param_t p = tree_param(t, i);

      for (int j = 0; j < n_overloads; j++) {
         if (overloads[j] != NULL) {
            type_t ptype = sem_find_param_type(p, overloads[j]);
            if (ptype != NULL)
               type_set_add(ptype);
         }
      }

      bool ok = sem_check(p.value);

      type_set_pop();

      if (!ok)
         return false;
   }

   for (int n = 0; n < n_overloads; n++) {
      if (overloads[n] == NULL)
         continue;

      // Did argument types match for this overload?
      bool match = true;
      bool all_universal = true;
      for (int i = 0; i < nparams; i++) {
         param_t p = tree_param(t, i);
         type_t ptype = tree_type(p.value);
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

      bool found = false;
      for (int j = 0; (j < nparams) && !found; j++) {
         param_t p = tree_param(call, j);
         switch (p.kind) {
         case P_POS:
            found = (p.pos == i);
            break;
         case P_NAMED:
            found = (p.name == name);
            break;
         default:
            assert(false);
         }
      }

      if (!found) {
         if (tree_has_value(port)) {
            param_t param = {
               .kind  = P_NAMED,
               .value = tree_value(port)
            };
            param.name = name;
            tree_add_param(call, param);
         }
         else
            sem_error(call, "missing actual for formal %s without "
                      "default value", istr(name));
      }
   }

   return true;
}

static bool sem_check_params(tree_t t)
{
   bool have_named = false;
   const int nparams = tree_params(t);
   for (int i = 0; i < nparams; i++) {
      param_t p = tree_param(t, i);

      switch (p.kind) {
      case P_POS:
         if (have_named)
            sem_error(p.value, "positional parameters must precede named "
                      "parameters");
         break;

      case P_NAMED:
         for (int j = 0; j < i; j++) {
            param_t q = tree_param(t, j);
            if ((q.kind == P_NAMED) && (q.name == p.name))
               sem_error(p.value, "duplicate parameter name %s", istr(p.name));
         }

         have_named = true;
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
   int n = 0, found_func = 0;
   do {
      if ((decl = scope_find_nth(tree_ident(t), n++))) {
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
               tree_set_ident(ref, tree_ident(t));

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
               if (type_eq(tree_type(overloads[i]), func_type))
                  duplicate = true;
            }

            if (!duplicate) {
               // Found a matching function definition
               overloads[n_overloads++] = decl;
            }
         }
      }
   } while (decl != NULL);

   if (n_overloads == 0)
      sem_error(t, (found_func > 0
                    ? "no matching function %s"
                    : "undefined identifier %s"),
                istr(tree_ident(t)));

   int matches;
   if (!sem_resolve_overload(t, &decl, &matches, overloads, n_overloads))
      return false;

   if (matches > 0 && decl == NULL)
      return true;   // Resolved to a builtin function

   if (matches > 1) {
      char buf[1024];
      static_printf_begin(buf, sizeof(buf));

      const bool operator = !isalpha((uint8_t)*istr(tree_ident(t)));

      for (int n = 0; n < n_overloads; n++) {
         if (overloads[n] != NULL)
            static_printf(buf, "\n%s",
                          sem_type_str(tree_type(overloads[n])));
      }

      sem_error(t, "ambiguous %s %s%s",
                operator ? "use of operator" : "call to function",
                istr(tree_ident(t)), buf);
   }

   if (decl == NULL) {
      char fn[512];
      static_printf_begin(fn, sizeof(fn));

      const char *fname = istr(tree_ident(t));
      const bool operator = !isalpha((uint8_t)fname[0]);
      const char *quote = (operator && fname[0] != '"') ? "\"" : "";

      static_printf(fn, "%s%s%s(", quote, fname, quote);
      for (unsigned i = 0; i < tree_params(t); i++)
         static_printf(fn, "%s%s",
                       (i == 0 ? "" : ", "),
                       sem_type_str(tree_type(tree_param(t, i).value)));
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
                       sem_type_str(tree_type(tree_param(t, i).value)));
      static_printf(fn, ")");

      sem_error(t, (n == 1 ? "undefined procedure %s"
                    : "no suitable overload for procedure %s"),
                fn);
   }

   const int nparams = tree_params(t);
   for (int i = 0; i < nparams; i++) {
      param_t param    = tree_param(t, i);
      tree_t  port     = tree_port(decl, i);
      class_t class    = tree_class(port);
      port_mode_t mode = tree_port_mode(port);

      tree_t value = param.value;
      tree_kind_t kind = tree_kind(value);
      while ((kind == T_ARRAY_REF) || (kind == T_ARRAY_SLICE)
             || (kind == T_ALL) || (kind == T_RECORD_REF)) {
         value = tree_value(value);
         kind  = tree_kind(value);
      }

      if (class == C_VARIABLE) {
         if (kind != T_REF)
            sem_error(param.value, "cannot associate this expression with "
                      "parameter class VARIABLE");

         tree_t decl = tree_ref(value);
         tree_kind_t decl_kind = tree_kind(decl);

         if (decl_kind == T_SIGNAL_DECL)
            sem_error(param.value, "cannot associate signal %s with parameter "
                      "class VARIABLE", istr(tree_ident(decl)));
         else if (decl_kind == T_FILE_DECL)
            sem_error(param.value, "cannot associate file %s with parameter "
                      "class VARIABLE", istr(tree_ident(decl)));
         else if (decl_kind == T_PORT_DECL) {
            if ((mode == PORT_OUT) && (tree_port_mode(decl) != PORT_OUT))
               sem_error(param.value, "cannot read parameter %s with mode OUT",
                         istr(tree_ident(decl)));
            else if (((mode == PORT_OUT) || (mode == PORT_INOUT))
                     && (tree_class(decl) == C_CONSTANT))
               sem_error(param.value, "object %s has class CONSTANT and "
                         "cannot be associated with OUT or INOUT parameters",
                         istr(tree_ident(decl)));
         }
         else if (decl_kind != T_VAR_DECL)
            sem_error(param.value, "invalid use of name %s",
                      istr(tree_ident(decl)));
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
   if (type_kind(type) == T_UARRAY)
      return type_index_constr(type, dim);
   else
      return tree_type(type_dim(type, dim).left);
}

static bool sem_check_concat_param(tree_t t)
{
   struct type_set *old = top_type_set;

   type_set_push();

   for (unsigned i = 0; i < old->n_members; i++) {
      type_t this = type_base_recur(old->members[i]);

      type_kind_t kind = type_kind(this);

      if (kind == T_CARRAY) {
         // The bounds of one side should not be used to determine
         // those of the other side
         type_t u = type_new(T_UARRAY);
         type_set_elem(u, type_elem(this));
         type_set_ident(u, type_ident(this));
         for (unsigned i = 0; i < type_dims(this); i++)
            type_add_index_constr(u, tree_type(type_dim(this, i).left));
         type_set_add(u);
      }
      else
         type_set_add(this);

      if (type_is_array(this))
         type_set_add(type_elem(this));
   }

   bool ok = sem_check(t);
   type_set_pop();

   return ok;
}

static bool sem_is_composite(type_t t)
{
   return type_is_array(t) || (type_kind(t) == T_RECORD);
}

static bool sem_check_concat(tree_t t)
{
   // Concatenation expressions are treated differently to other operators
   // as they have special rules. See LRM 93 section 9.2.5

   assert(tree_params(t) == 2);
   tree_t left  = tree_param(t, 0).value;
   tree_t right = tree_param(t, 1).value;

   if (!type_set_restrict(sem_is_composite))
      sem_error(t, "no composite type in context%s", type_set_fmt());

   if (sem_maybe_ambiguous(left)) {
      if (!sem_check_concat_param(right) || !sem_check_concat_param(left))
         return false;
   }
   else {
      if (!sem_check_concat_param(right) || !sem_check_concat_param(left))
         return false;
   }

   type_t ltype = tree_type(left);
   type_t rtype = tree_type(right);

   type_kind_t lkind = type_kind(ltype);
   type_kind_t rkind = type_kind(rtype);

   bool l_array = type_is_array(ltype);
   bool r_array = type_is_array(rtype);

   if (l_array && r_array) {
      if (!type_eq(ltype, rtype))
         sem_error(t, "cannot concatenate arrays of different types");

      if (sem_array_dimension(ltype) > 1)
         sem_error(t, "cannot concatenate arrays with more than one dimension");

      type_t index_type = sem_index_type(ltype, 0);
      range_t index_r = type_dim(index_type, 0);

      type_t std_int = sem_std_type("INTEGER");
      tree_t left_len, right_len;

      if (lkind == T_UARRAY)
         left_len = call_builtin("length", std_int, left, NULL);
      else
         left_len = sem_array_len(ltype);

      if (rkind == T_UARRAY)
         right_len = call_builtin("length", std_int, right, NULL);
      else
         right_len = sem_array_len(rtype);

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
         array_len = call_builtin("length", std_int, array, NULL);
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
   if (tree_has_type(t))
      return true;

   literal_t l = tree_literal(t);

   switch (l.kind) {
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
      assoc_t a = tree_assoc(t, i);

      switch (a.kind) {
      case A_POS:
         if (state > POS)
            sem_error(a.value, "positional associations must appear "
                      "first in aggregate");
         have_pos = true;
         break;

      case A_NAMED:
      case A_RANGE:
         if (state > NAMED)
            sem_error(a.name, "named association must not follow "
                      "others association in aggregate");
         state = NAMED;
         have_named = true;
         break;

      case A_OTHERS:
         if (state == OTHERS)
            sem_error(a.value, "only a single others association "
                      "allowed in aggregate");
         if (type_kind(composite_type) == T_UARRAY)
            sem_error(a.value, "others choice not allowed in this context");
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

   if (type_kind(composite_type) == T_UARRAY) {
      type_t tmp = type_new(T_CARRAY);
      type_set_ident(tmp, type_ident(composite_type));
      type_set_elem(tmp, type_elem(composite_type));  // Element type

      assert(type_index_constrs(composite_type) == 1);  // TODO

      type_t index_type = type_index_constr(composite_type, 0);
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
         tree_t n_elems = sem_make_int(tree_assocs(t) - 1);

         range_t r = {
            .kind  = index_r.kind,
            .left  = index_r.left,
            .right = call_builtin("add", index_type, n_elems,
                                  index_r.left, NULL)
         };
         type_add_dim(tmp, r);
      }

      composite_type = tmp;
   }

   // All elements must be of the composite base type if this is
   // a one-dimensional array otherwise construct an array type
   // with n-1 dimensions.

   if (type_is_array(base_type)) {
      type_t elem_type = NULL;
      if (type_dims(composite_type) == 1)
         elem_type = type_elem(base_type);
      else {
         elem_type = type_new(T_CARRAY);
         type_set_ident(elem_type, type_ident(composite_type));
         type_set_elem(elem_type, type_elem(base_type));

         for (unsigned i = 1; i < type_dims(composite_type); i++)
            type_add_dim(elem_type, type_dim(composite_type, i));
      }

      type_t index_type = sem_index_type(composite_type, 0);

      for (int i = 0; i < nassocs; i++) {
         assoc_t a = tree_assoc(t, i);

         switch (a.kind) {
         case A_RANGE:
            if (!sem_check_range(&a.range, index_type))
               return false;
            tree_change_assoc(t, i, a);
            break;

         case A_NAMED:
            if (!sem_check_constrained(a.name, index_type))
               return false;
            break;

         default:
            break;
         }

         if (!sem_check_constrained(a.value, elem_type))
            return false;

         if (!type_eq(elem_type, tree_type(a.value)))
            sem_error(a.value, "type of element %s does not match base "
                      "type of aggregate %s",
                      sem_type_str(tree_type(a.value)),
                      sem_type_str(elem_type));
      }
   }

   // Checks for record aggregates are given in LRM 93 section 7.3.2.1

   if (type_kind(base_type) == T_RECORD) {
      const int nfields = type_fields(base_type);
      bool have[nfields];
      int pos = 0;
      for (int i = 0; i < nfields; i++)
         have[i] = false;

      for (int i = 0; i < nassocs; i++) {
         assoc_t a = tree_assoc(t, i);
         int f = -1;

         switch (a.kind) {
         case A_NAMED:
            {
               if (tree_kind(a.name) != T_REF)
                  sem_error(a.name, "association name must be a field "
                            "identifier");

               ident_t name_i = tree_ident(a.name);
               for (f = 0; f < nfields; f++) {
                  tree_t field = type_field(base_type, f);
                  if (tree_ident(field) == name_i) {
                     tree_set_type(a.name, tree_type(field));
                     tree_set_ref(a.name, field);
                     break;
                  }
               }

               if (f == nfields)
                  sem_error(a.name, "type %s does not have field named %s",
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
            sem_error(a.value, "range is not allowed here");
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
                  sem_error(a.value, "field %s already has a value",
                            istr(tree_ident(field)));
            }

            if (!sem_check_constrained(a.value, field_type))
               return false;

            if (!type_eq(field_type, tree_type(a.value)))
               sem_error(a.value, "type of value %s does not match type "
                         "of field %s %s",
                         sem_type_str(tree_type(a.value)),
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

   for (unsigned i = 0; i < tree_assocs(t); i++) {
      assoc_t a = tree_assoc(t, i);
      if (a.kind == A_NAMED && !sem_locally_static(a.name)) {
         if (tree_assocs(t) != 1)
            sem_error(a.name, "non-locally static choice must be "
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

   type_kind_t kind = type_kind(tree_type(rec));
   if (kind == T_ACCESS) {
      // Record fields can be dereferenced implicitly
      value = tree_new(T_ALL);
      tree_set_value(value, sem_make_ref(rec));
      tree_set_type(value, type_access(tree_type(rec)));
   }
   else
      assert(kind == T_RECORD);

   if (tree_kind(rec) == T_FIELD_DECL) {
      value = tree_new(T_REF);
      tree_set_loc(value, tree_loc(t));
      tree_set_ident(value, base);
      sem_convert_to_record_ref(value, rec);
   }
   else if (value == NULL)
      value = sem_make_ref(rec);

   tree_change_kind(t, T_RECORD_REF);
   tree_set_value(t, value);
   tree_set_ident(t, tree_ident(decl));
   tree_set_type(t, tree_type(decl));
}

static bool sem_check_ref(tree_t t)
{
   tree_t decl;
   int n = 0;
   do {
      if ((decl = scope_find_nth(tree_ident(t), n++))) {
         type_t type = tree_type(decl);
         if (type_set_member(type))
            break;
         else if (type_kind(type) == T_FUNC
                  && (sem_required_args(decl) == 0)
                  && type_set_member(type_result(type)))
            // Zero-argument function of correct type
            break;
         else if (!scope_can_overload(decl))
            break;
      }
   } while (decl != NULL);

   if (decl == NULL)
      sem_error(t, (n == 1 ? "undefined identifier %s"
                    : "no suitable overload for identifier %s"),
                istr(tree_ident(t)));

   switch (tree_kind(decl)) {
   case T_VAR_DECL:
   case T_SIGNAL_DECL:
   case T_PORT_DECL:
   case T_CONST_DECL:
   case T_ENUM_LIT:
   case T_ALIAS:
   case T_FILE_DECL:
   case T_UNIT_DECL:
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

static type_t sem_implicit_dereference(tree_t t)
{
   // Construct the implicit dereference when slicing or indexing arrays
   // through accesses

   tree_t value = tree_value(t);

   type_t type = tree_type(value);
   assert(type_kind(type) == T_ACCESS);

   type_t access = type_access(type);

   tree_t all = tree_new(T_ALL);
   tree_set_loc(all, tree_loc(value));
   tree_set_value(all, value);
   tree_set_type(all, access);

   tree_set_value(t, all);

   return access;
}

static bool sem_check_array_ref(tree_t t)
{
   tree_t value = tree_value(t);
   if (!sem_check_constrained(value, NULL))
      return false;

   type_t type = tree_type(tree_value(t));

   if (type_kind(type) == T_ACCESS)
      type = sem_implicit_dereference(t);

   if (!type_is_array(type))
      sem_error(t, "invalid array reference");

   const int nindex = (type_kind(type) == T_UARRAY
                       ? type_index_constrs(type)
                       : type_dims(type));

   const int nparams = tree_params(t);

   if (nparams != nindex)
      sem_error(t, "array %s has %d dimensions but %d indices given",
                istr(tree_ident(value)), nindex, nparams);

   bool ok = true;
   for (int i = 0; i < nparams; i++) {
      param_t p = tree_param(t, i);
      if (p.kind != P_POS)
         sem_error(t, "only scalar references supported");

      type_t expect = sem_index_type(type, i);

      ok = sem_check_constrained(p.value, expect) && ok;

      if (ok && !type_eq(expect, tree_type(p.value)))
         sem_error(p.value, "type of index %s does not match type of "
                   "array dimension %s",
                   sem_type_str(tree_type(p.value)),
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
      array_type = sem_implicit_dereference(t);

   if (!type_is_array(array_type))
      sem_error(t, "type of slice prefix is not an array");

   range_t r = tree_range(t);
   if (!sem_check_range(&r, sem_std_type("INTEGER")))
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
   tree_t decl = scope_find(tree_ident(t));

   if (decl == NULL)
      sem_error(t, "undefined identifier %s", istr(tree_ident(t)));

   ident_t attr = tree_ident2(t);

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
                istr(tree_ident(t)), istr(attr));

   if (tree_kind(a) == T_FUNC_DECL) {
      type_t ftype = tree_type(a);
      tree_set_type(t, type_result(ftype));

      ident_t pname = ident_new("_arg0");

      // For an expression X'A add X as a hidden parameter
      bool already_added = false;
      for (unsigned i = 0; (i < tree_params(t)) && !already_added; i++) {
         param_t p = tree_param(t, i);
         if ((p.kind == P_NAMED) && (p.name == pname))
            already_added = true;
      }

      if (!already_added && (tree_params(t) == 0)) {
         tree_t ref = sem_make_ref(decl);
         tree_set_loc(ref, tree_loc(t));

         param_t p = { .kind = P_NAMED, .value = ref };
         p.name = pname;
         tree_add_param(t, p);
      }

      const int nparams = tree_params(t);

      if (nparams != type_params(ftype))
         sem_error(t, "expected %d parameters for attribute %s "
                   "but have %d", type_params(ftype), istr(attr), nparams);

      for (int i = 0; i < nparams; i++) {
         param_t p = tree_param(t, i);

         if ((p.kind == P_NAMED) && (p.name == pname))
            continue;

         if (p.kind != P_POS)
            sem_error(t, "only positional arguments supported here");

         type_t expect_type = type_param(ftype, i);
         if (!sem_check_constrained(p.value, expect_type))
            return false;

         if (!type_eq(tree_type(p.value), expect_type))
            sem_error(t, "expected type %s for attribute %s",
                      sem_type_str(expect_type), istr(attr));
      }

      tree_set_ref(t, a);
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

   type_set_force(tree_type(decl));
   tree_set_type(t, tree_type(decl));
   return sem_check(tree_value(t));
}

static bool sem_check_map(tree_t t, tree_t unit,
                          tree_formals_t tree_Fs, tree_formal_t tree_F,
                          tree_actuals_t tree_As, tree_actual_t tree_A)
{
   // Check there is an actual for each formal port or generic

   const unsigned nformals = tree_Fs(unit);
   bool ok = true;

   struct {
      tree_t decl;
      bool   have;
   } formals[nformals];

   for (unsigned i = 0; i < nformals; i++) {
      formals[i].decl = tree_F(unit, i);
      formals[i].have = false;
   }

   for (unsigned i = 0; i < tree_As(t); i++) {
      param_t p = tree_A(t, i);
      tree_t decl = NULL;
      switch (p.kind) {
      case P_POS:
         if (p.pos >= nformals)
            sem_error(p.value, "too many positional actuals");
         if (formals[p.pos].have)
            sem_error(p.value, "formal %s already has an actual",
                      istr(tree_ident(formals[p.pos].decl)));
         formals[p.pos].have = true;
         decl = formals[p.pos].decl;
         break;

      case P_NAMED:
         for (unsigned i = 0; i < nformals; i++) {
            if (tree_ident(formals[i].decl) == p.name) {
               if (formals[i].have)
                  sem_error(p.value, "formal %s already has an actual",
                            istr(tree_ident(formals[i].decl)));
               formals[i].have = true;
               decl = formals[i].decl;
               break;
            }
         }

         if (decl == NULL)
            sem_error(p.value, "%s has no formal %s",
                      istr(tree_ident(unit)), istr(p.name));
         break;
      }

      ok = sem_check_constrained(p.value, tree_type(decl)) && ok;

      if ((tree_kind(p.value) == T_OPEN) && (tree_port_mode(decl) != PORT_OUT))
         sem_error(p.value, "OPEN can only be used with OUT ports");
   }

   for (unsigned i = 0; i < nformals; i++) {
      if (!formals[i].have && !tree_has_value(formals[i].decl))
         sem_error(t, "missing actual for formal %s",
                   istr(tree_ident(formals[i].decl)));
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
         unit = lib_get(lib_work(), name);
         if (unit == NULL)
            sem_error(t, "cannot find unit %s", istr(name));

         if (tree_kind(unit) == T_ARCH) {
            unit = lib_get(lib_work(), ident_until(name, '-'));
            if (unit == NULL)
               sem_error(t, "no entity corresponding to architecture %s",
                         istr(name));
         }
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

   bool ok = true;
   for (unsigned i = 0; i < tree_stmts(t); i++)
      ok = sem_check(tree_stmt(t, i)) && ok;
   for (unsigned i = 0; i < tree_else_stmts(t); i++)
      ok = sem_check(tree_else_stmt(t, i)) && ok;

   return ok;
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
      for (unsigned i = 0; i < tree_params(t); i++) {
         param_t p = tree_param(t, i);
         all_static = all_static && sem_locally_static(p.value);
      }
      return all_static;
   }

   // TODO: clauses e, f, and g re. attributes

   // A qualified expression whose operand is locally static
   if (kind == T_QUALIFIED)
      return sem_locally_static(tree_value(t));

   // A type conversion whose expression is locally static
   if (kind == T_TYPE_CONV)
      return sem_locally_static(tree_value(t));

   // Aggregates must have locally static range and all elements
   // must have locally static values
   if (kind == T_AGGREGATE) {
      range_t r = type_dim(type, 0);
      if (r.kind != RANGE_TO && r.kind != RANGE_DOWNTO)
         return false;

      if (!sem_locally_static(r.left) || !sem_locally_static(r.right))
         return false;

      for (unsigned i = 0; i < tree_assocs(t); i++) {
         assoc_t a = tree_assoc(t, i);
         if ((a.kind == A_NAMED) && !sem_locally_static(a.name))
            return false;

         if (!sem_locally_static(a.value))
            return false;
      }

      return true;
   }

   // A record field name
   if ((kind == T_REF) && (tree_kind(tree_ref(t)) == T_FIELD_DECL))
      return true;

   return false;
}

static bool sem_check_case(tree_t t)
{
   tree_t test = tree_value(t);
   if (!sem_check(test))
      return false;

   type_t type = tree_type(test);

   bool ok = true;
   for (unsigned i = 0; i < tree_assocs(t); i++) {
      assoc_t a = tree_assoc(t, i);
      switch (a.kind) {
      case A_OTHERS:
         if (i != tree_assocs(t) - 1)
            sem_error(t, "others choice must appear last");
         break;

      case A_NAMED:
         ok = sem_check_constrained(a.name, type) && ok;
         if (ok) {
            if (!type_eq(tree_type(a.name), type))
               sem_error(a.name, "case choice must have type %s",
                         sem_type_str(type));
            if (!sem_locally_static(a.name))
               sem_error(a.name, "case choice must be locally static");
         }
         break;

      default:
         assert(false);
      }

      ok = sem_check(a.value) && ok;
   }

   if (!ok)
      return false;

   // Check the choices cover all elements of an enumerated type
   if (type_kind(type) == T_ENUM) {
      unsigned nlits = type_enum_literals(type);
      bool have[nlits];
      for (unsigned i = 0; i < nlits; i++)
         have[i] = false;

      bool have_others = false;

      for (unsigned i = 0; i < tree_assocs(t); i++) {
         assoc_t a = tree_assoc(t, i);

         if (a.kind == A_OTHERS) {
            have_others = true;
            continue;
         }

         ident_t name = tree_ident(a.name);
         for (unsigned j = 0; j < nlits; j++) {
            if (tree_ident(type_enum_literal(type, j)) == name) {
               if (have[j])
                  sem_error(a.name, "choice %s appears multiple times "
                            "in case statement", istr(name));
               else
                  have[j] = true;
            }
         }
      }

      bool have_all = true;
      for (unsigned i = 0; i < nlits; i++) {
         if (!have[i] && !have_others)
            sem_error(t, "missing choice %s in case statement",
                      istr(tree_ident(type_enum_literal(type, i))));
         have_all = have_all && have[i];
      }

      if (have_all && have_others)
         warn_at(tree_loc(t), "OTHERS choice is redundant as named "
                 "choices cover all cases");
   }

   return true;
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

   bool ok = true;
   const int nstmts = tree_stmts(t);
   for (int i = 0; i < nstmts; i++)
      ok = sem_check(tree_stmt(t, i)) && ok;

   loop_pop();

   return ok;
}

static bool sem_check_for(tree_t t)
{
   range_t r = tree_range(t);
   if (!sem_check_range(&r, NULL))
      return false;
   tree_set_range(t, r);

   tree_t idecl = tree_new(T_VAR_DECL);
   tree_set_ident(idecl, tree_ident2(t));
   tree_set_loc(idecl, tree_loc(t));
   tree_set_type(idecl, tree_type(r.left));

   tree_add_decl(t, idecl);

   scope_push(NULL);
   scope_insert(idecl);
   loop_push(tree_ident(t));

   bool ok = true;
   for (unsigned i = 0; i < tree_stmts(t); i++)
      ok = sem_check(tree_stmt(t, i)) && ok;

   loop_pop();
   scope_pop();
   return ok;
}

static bool sem_check_block(tree_t t)
{
   scope_push(NULL);

   bool ok = true;

   for (unsigned i = 0; i < tree_decls(t); i++)
      ok = sem_check(tree_decl(t, i)) && ok;

   for (unsigned i = 0; i < tree_stmts(t); i++)
      ok = sem_check(tree_stmt(t, i)) && ok;

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

   for (unsigned i = 0; i < tree_assocs(t); i++) {
      assoc_t a = tree_assoc(t, i);
      if (a.kind == A_NAMED) {
         if (!sem_check_constrained(a.name, value_type))
            return false;
         else if (!type_eq(tree_type(a.name), value_type))
            sem_error(a.name, "choice must have type %s", sem_type_str(value_type));
         else if (!sem_locally_static(a.name))
            sem_error(a.name, "choice must be locally static");
      }

      if (!sem_check(a.value))
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

   tree_t obj_decl = scope_find(tree_ident2(t));
   if (obj_decl == NULL)
      sem_error(t, "undefined identifier %s", istr(tree_ident2(t)));

   if (tree_kind(attr_decl) != T_ATTR_DECL)
      sem_error(t, "name %s is not an attribute declaration",
                istr(tree_ident(t)));

   type_t type = tree_type(attr_decl);

   tree_t value = tree_value(t);
   if (!sem_check_constrained(value, type))
      return false;

   if (!type_eq(type, tree_type(value)))
      sem_error(t, "expected attribute type %s", sem_type_str(type));

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

   scope_push(NULL);

   bool ok = true;

   for (unsigned i = 0; i < tree_decls(t); i++)
      ok = sem_check(tree_decl(t, i)) && ok;

   if (ok) {
      for (unsigned i = 0; i < tree_stmts(t); i++)
         ok = sem_check(tree_stmt(t, i)) && ok;
   }

   scope_pop();
   return ok;
}

static bool sem_check_for_generate(tree_t t)
{
   range_t r = tree_range(t);
   if (!sem_check_range(&r, NULL))
      return false;
   tree_set_range(t, r);

   tree_t idecl = tree_new(T_VAR_DECL);
   tree_set_ident(idecl, tree_ident2(t));
   tree_set_loc(idecl, tree_loc(t));
   tree_set_type(idecl, tree_type(r.left));

   tree_set_ref(t, idecl);

   scope_push(NULL);
   scope_insert(idecl);

   bool ok = true;

   for (unsigned i = 0; i < tree_decls(t); i++)
      ok = sem_check(tree_decl(t, i)) && ok;

   if (ok) {
      for (unsigned i = 0; i < tree_stmts(t); i++)
         ok = sem_check(tree_stmt(t, i)) && ok;
   }

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

   type_t type;
   if (!type_set_uniq(&type))
      sem_error(t, "context does not contain unique access type");

   switch (tree_kind(value)) {
   case T_ARRAY_SLICE:
      {
         range_t r = tree_range(value);
         if (!sem_check_range(&r, sem_std_type("INTEGER")))
            return false;

         value = tree_value(value);
         if (tree_kind(value) != T_REF)
            sem_error(t, "invalid array allocator expression");
      }
      // Fall-through

   case T_REF:
      {
         tree_t decl = scope_find(tree_ident(value));
         if ((decl == NULL) || (tree_kind(decl) != T_TYPE_DECL))
            sem_error(value, "%s does not name a type",
                      istr(tree_ident(value)));

         tree_set_ref(value, decl);
         tree_set_type(value, tree_type(decl));
      }
      break;

   case T_QUALIFIED:
      break;

   default:
      sem_error(t, "invalid allocator expression");
   }

   tree_set_type(t, type);
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

   builtin_i      = ident_new("builtin");
   std_standard_i = ident_new("STD.STANDARD");
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
   default:
      sem_error(t, "cannot check %s", tree_kind_str(tree_kind(t)));
   }
}

int sem_errors(void)
{
   return errors;
}

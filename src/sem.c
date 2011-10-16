//
//  Copyright (C) 2011  Nick Gasson
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

#include <assert.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>
#include <ctype.h>

struct ident_list {
   ident_t           ident;
   struct ident_list *next;
};

struct btree {
   tree_t       tree;
   struct btree *left;
   struct btree *right;
};

struct scope {
   struct btree      *decls;

   // For design unit scopes
   ident_t           prefix;
   struct ident_list *context;
   struct ident_list *imported;

   struct scope      *down;
};

#define MAX_OVERLOADS 32

#define MAX_TS_MEMBERS 16
struct type_set {
   type_t          members[MAX_TS_MEMBERS];
   unsigned        n_members;

   struct type_set *down;
};

static void sem_declare_predefined_ops(tree_t decl);
static bool sem_check_constrained(tree_t t, type_t type);
static bool sem_check_array_ref(tree_t t);

static struct scope    *top_scope = NULL;
static int             errors = 0;
static struct type_set *top_type_set = NULL;
static bool            bootstrap = false;

#define sem_error(t, ...) {               \
   error_at(tree_loc(t), __VA_ARGS__);    \
   errors++;                              \
   return false;                          \
}

static void scope_push(ident_t prefix)
{
   struct scope *s = xmalloc(sizeof(struct scope));
   s->decls    = NULL;
   s->prefix   = prefix;
   s->context  = NULL;
   s->imported = NULL;
   s->down     = top_scope;

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

   scope_ident_list_free(top_scope->context);
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

static void scope_add_context(ident_t prefix)
{
   assert(top_scope != NULL);

   scope_ident_list_add(&top_scope->context, prefix);
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
   printf("%-30s%s\n", istr(tree_ident(b->tree)),
          type_pp(tree_type(b->tree)));
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

static bool scope_btree_cmp(ident_t a, ident_t b)
{
   // We can't compare identifier pointers directly as these may be
   // prefixed during a search so we compare the final character with
   // a hack to make character identifiers behave better

   char a_final = ident_char(a, 0);
   char b_final = ident_char(b, 0);
   if (a_final == '\'' && b_final == '\'') {
      a_final = ident_char(a, 1);
      b_final = ident_char(b, 1);
   }

   return a_final < b_final;
}

static tree_t scope_find_in(ident_t i, struct scope *s, bool recur, int k)
{
   if (s == NULL)
      return NULL;
   else {
      struct btree *search = s->decls;

      while (search != NULL) {
         ident_t this = tree_ident(search->tree);

         if (this == i
             || (s->prefix != NULL
                 && this == ident_prefix(s->prefix, i, '.'))) {
            if (k == 0)
               return search->tree;
            else
               --k;
         }
         else {
            struct ident_list *it;
            for (it = s->context; it != NULL; it = it->next) {
               if (this == ident_prefix(it->ident, i, '.')) {
                  if (k == 0)
                     return search->tree;
                  else
                     --k;
               }
            }
         }

         bool left = scope_btree_cmp(i, this);
         search = (left ? search->left : search->right);
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

static bool can_overload(tree_t t)
{
   return tree_kind(t) == T_ENUM_LIT
      || tree_kind(t) == T_FUNC_DECL;
}

static struct btree *scope_btree_new(tree_t t)
{
   struct btree *b = xmalloc(sizeof(struct btree));
   b->tree  = t;
   b->left  = NULL;
   b->right = NULL;

   return b;
}

static void scope_insert_at(tree_t t, struct btree *where)
{
   bool left = scope_btree_cmp(tree_ident(t), tree_ident(where->tree));

   struct btree **nextp = (left ? &where->left : &where->right);

   if (*nextp == NULL)
      *nextp = scope_btree_new(t);
   else
      scope_insert_at(t, *nextp);
}

static bool scope_insert(tree_t t)
{
   assert(top_scope != NULL);

   if (!can_overload(t)
       && scope_find_in(tree_ident(t), top_scope, false, 0))
      sem_error(t, "%s already declared in this scope",
                istr(tree_ident(t)));

   if (top_scope->decls == NULL)
      top_scope->decls = scope_btree_new(t);
   else
      scope_insert_at(t, top_scope->decls);
   return true;
}

static bool scope_insert_special(tree_t t)
{
   // Handle special cases of scope insertion such as enumeration
   // literals, physical unit names, and predefined types

   bool ok = scope_insert(t);

   if (tree_kind(t) == T_TYPE_DECL)
      sem_declare_predefined_ops(t);

   type_t type = tree_type(t);
   switch (type_kind(type)) {
   case T_ENUM:
      // Need to add each literal to the scope
      for (unsigned i = 0; i < type_enum_literals(type); i++)
         ok = ok && scope_insert(type_enum_literal(type, i));
      break;

   case T_PHYSICAL:
      // Create constant declarations for each unit
      for (unsigned i = 0; i < type_units(type); i++) {
         unit_t u = type_unit(type, i);
         ok = ok && sem_check_constrained(u.multiplier, type);

         tree_set_type(u.multiplier, type);

         tree_t c = tree_new(T_CONST_DECL);
         tree_set_loc(c, tree_loc(u.multiplier));
         tree_set_ident(c, u.name);
         tree_set_type(c, type);
         tree_set_value(c, u.multiplier);

         ok = ok && scope_insert(c);
      }
      break;

   default:
      break;
   }

   return ok;
}

static bool scope_import_unit(lib_t lib, ident_t name)
{
   // Check we haven't already imported this
   struct ident_list *it;
   for (it = top_scope->imported; it != NULL; it = it->next) {
      if (it->ident == name)
         return true;
   }

   tree_t unit = lib_get(lib, name);
   if (unit == NULL)
      sem_error(NULL, "unit %s not found in library %s",
                istr(name), istr(lib_name(lib)));

   for (unsigned n = 0; n < tree_decls(unit); n++) {
      if (!scope_insert_special(tree_decl(unit, n)))
         return false;
   }

   scope_ident_list_add(&top_scope->imported, name);
   return true;
}

static void type_set_push(void)
{
   struct type_set *t = xmalloc(sizeof(struct type_set));
   t->n_members = 0;
   t->down      = top_type_set;

   top_type_set = t;
}

static void type_set_pop(void)
{
   assert(top_type_set != NULL);

   struct type_set *old = top_type_set;
   top_type_set = old->down;
   free(old);
}

static void type_set_add(type_t t)
{
   assert(top_type_set != NULL);
   assert(top_type_set->n_members < MAX_TS_MEMBERS);
   assert(t != NULL);
   assert(type_kind(t) != T_UNRESOLVED);

   for (unsigned i = 0; i < top_type_set->n_members; i++) {
      if (top_type_set->members[i] == t)
         return;
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

static bool type_set_uniq(type_t *pt)
{
   assert(top_type_set != NULL);

   if (top_type_set->n_members == 1) {
      *pt = top_type_set->members[0];
      return true;
   }
   else
      return false;
}

#if 0
static void type_set_dump(void)
{
   printf("type_set: { ");
   if (top_type_set) {
      for (unsigned n = 0; n < top_type_set->n_members; n++)
         printf("%s ", istr(type_ident(top_type_set->members[n])));
   }
   printf("}\n");
}
#endif

static bool type_set_member(type_t t)
{
   if (top_type_set == NULL)
      return true;

   for (unsigned n = 0; n < top_type_set->n_members; n++) {
      if (type_eq(top_type_set->members[n], t))
         return true;
   }

   return false;
}

static type_t sem_std_type(const char *name)
{
   tree_t decl = scope_find(ident_new(name));
   if (decl == NULL)
      fatal("cannot find %s type", name);

   return tree_type(decl);
}

static tree_t sem_builtin_fn(ident_t name, type_t result,
                             const char *builtin)
{
   type_t f = type_new(T_FUNC);
   type_set_ident(f, name);
   type_set_result(f, result);

   tree_t d = tree_new(T_FUNC_DECL);
   tree_set_ident(d, name);
   tree_set_type(d, f);
   tree_add_attr_str(d, ident_new("builtin"), builtin);

   return d;
}

static void sem_declare_binary(ident_t name, type_t lhs, type_t rhs,
                               type_t result, const char *builtin)
{
   tree_t d = sem_builtin_fn(name, result, builtin);
   type_add_param(tree_type(d), lhs);
   type_add_param(tree_type(d), rhs);

   scope_insert(d);
}

static void sem_declare_unary(ident_t name, type_t operand,
                              type_t result, const char *builtin)
{
   tree_t d = sem_builtin_fn(name, result, builtin);
   type_add_param(tree_type(d), operand);

   scope_insert(d);
}

static tree_t sem_bool_lit(type_t std_bool, bool v)
{
   tree_t lit = type_enum_literal(std_bool, v ? 1 : 0);

   tree_t b = tree_new(T_REF);
   tree_set_ref(b, lit);
   tree_set_type(b, std_bool);
   tree_set_ident(b, tree_ident(lit));

   return b;
}

static void sem_declare_predefined_ops(tree_t decl)
{
   // Prefined operators are defined in LRM 93 section 7.2

   type_t t = tree_type(decl);

   ident_t mult  = ident_new("*");
   ident_t div   = ident_new("/");
   ident_t plus  = ident_new("+");
   ident_t minus = ident_new("-");

   type_t std_int = NULL;
   type_t std_bool = NULL;

   // Predefined operators

   switch (type_kind(t)) {
   case T_PHYSICAL:
      // These types require INTEGER to be declared
      std_int = sem_std_type("STD.STANDARD.INTEGER");

      // Fall-through
   default:
      // These types require BOOLEAN to be declared
      std_bool = sem_std_type("STD.STANDARD.BOOLEAN");
   }

   switch (type_kind(t)) {
   case T_SUBTYPE:
      // Use operators of base type
      break;

   case T_CARRAY:
   case T_UARRAY:
      // Operators on arrays
      sem_declare_binary(ident_new("="), t, t, std_bool, "aeq");
      sem_declare_binary(ident_new("/="), t, t, std_bool, "aneq");
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

      // Fall-through
   case T_INTEGER:
      // Modulus
      sem_declare_binary(ident_new("MOD"), t, t, t, "mod");

      // Remainder
      sem_declare_binary(ident_new("REM"), t, t, t, "rem");

      // Fall-through
   case T_REAL:
      // Addition
      sem_declare_binary(plus, t, t, t, "add");
      //sem_declare_binary(plus, t, t, "add");

      // Subtraction
      sem_declare_binary(minus, t, t, t, "sub");
      //sem_declare_binary(minus, t, t, t, "sub");

      // Multiplication
      sem_declare_binary(mult, t, t, t, "mul");
      //sem_declare_binary(mult, t, t, t, "mul");

      // Division
      sem_declare_binary(div, t, t, t, "div");
      //sem_declare_binary(div, t, t, t, "div");

      // Sign operators
      sem_declare_unary(plus, t, t, "identity");
      sem_declare_unary(minus, t, t, "neg");

      // Fall-through
   case T_ENUM:
      sem_declare_binary(ident_new("<"), t, t, std_bool, "lt");
      sem_declare_binary(ident_new("<="), t, t, std_bool, "leq");
      sem_declare_binary(ident_new(">"), t, t, std_bool, "gt");
      sem_declare_binary(ident_new(">="), t, t, std_bool, "geq");

      // Fall-through
   default:
      sem_declare_binary(ident_new("="), t, t, std_bool, "eq");
      sem_declare_binary(ident_new("/="), t, t, std_bool, "neq");

      break;
   }

   // Logical operators

   bool logical =
      (type_ident(t) == ident_new("STD.STANDARD.BOOLEAN")
       || type_ident(t) == ident_new("STD.STANDARD.BIT"));
   // TODO: also any one-dimensional array type whose element type
   // is BIT or BOOLEAN

   if (logical) {
      sem_declare_binary(ident_new("and"), t, t, t, "and");
      sem_declare_binary(ident_new("or"), t, t, t, "or");
      sem_declare_binary(ident_new("xor"), t, t, t, "xor");
      sem_declare_binary(ident_new("nand"), t, t, t, "nand");
      sem_declare_binary(ident_new("nor"), t, t, t, "nor");
      sem_declare_binary(ident_new("xnor"), t, t, t, "xnor");
      sem_declare_unary(ident_new("not"), t, t, "not");
   }

   // Predefined attributes

   switch (type_kind(t)) {
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
      }
      break;

   default:
      break;
   }
}

static bool sem_check_context(tree_t t)
{
   ident_t work_name = lib_name(lib_work());

   // The work library should always be searched
   scope_add_context(work_name);

   // The std.standard package is also implicit unless we are
   // bootstrapping
   if (!bootstrap) {
      lib_t std = lib_find("std", true);
      if (std == NULL)
         fatal("failed to find std library");

      ident_t std_standard_name = ident_new("STD.STANDARD");
      scope_add_context(std_standard_name);
      if (!scope_import_unit(std, std_standard_name))
         return false;
   }

   for (unsigned n = 0; n < tree_contexts(t); n++) {
      ident_t c = tree_context(t, n);
      ident_t all = ident_strip(c, ident_new(".all"));
      if (all) {
         scope_add_context(all);
         c = all;
      }

      if (!scope_import_unit(lib_work(), c))
          return false;
   }

   return true;
}

static bool sem_check_constrained(tree_t t, type_t type)
{
   type_set_push();
   type_set_add(type);
   bool ok = sem_check(t);
   type_set_pop();
   return ok;
}

static tree_t sem_make_int(int i)
{
   literal_t l;
   l.kind = L_INT;
   l.i    = i;

   tree_t t = tree_new(T_LITERAL);
   tree_set_literal(t, l);

   return t;
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

static bool sem_check_subtype(tree_t t, type_t type, type_t *pbase)
{
   // Resolve a subtype to its base type

   while (type_kind(type) == T_SUBTYPE) {
      type_t base = type_base(type);
      if (type_kind(base) == T_UNRESOLVED) {
         tree_t base_decl = scope_find(type_ident(base));
         if (base_decl == NULL)
            sem_error(t, "type %s is not defined", istr(type_ident(base)));

         type_t base_type = tree_type(base_decl);
         type_set_base(type, base_type);

         // If the subtype is not constrained then give it the same
         // range as its base type
         if (type_dims(type) == 0) {
            for (unsigned i = 0; i < type_dims(base_type); i++)
               type_add_dim(type, type_dim(base_type, i));
         }

         base = tree_type(base_decl);
      }

      type = base;
   }

   if (pbase)
      *pbase = type;

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

               // TODO: check index constraints here

               type_t collapse = type_new(T_CARRAY);
               type_set_ident(collapse, type_ident(base));
               type_set_base(collapse, type_base(base));  // Element type

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
            sem_error(t, "type %s is not defined", istr(type_ident(*ptype)));

         *ptype = tree_type(type_decl);
      }
      return true;

   default:
      abort();
   }
}

static bool sem_check_type_decl(tree_t t)
{
   type_t type = tree_type(t);

   // Prefix the package name to the type name
   if (top_scope->prefix)
      type_set_ident(type, ident_prefix(top_scope->prefix,
                                        type_ident(type), '.'));

   type_t base;
   if (!sem_check_subtype(t, type, &base))
      return false;

   // We need to insert the type into the scope before performing
   // further checks as when bootstrapping we need INTEGER defined
   // before we can check any ranges. Adding a type with errors to
   // the symbol table should also avoid spurious type-not-defined
   // errors later on
   scope_apply_prefix(t);
   if (!scope_insert_special(t))
      return false;

   switch (type_kind(type)) {
   case T_UARRAY:
   case T_CARRAY:
      {
         type_t elem_type = type_base(base);
         if (!sem_check_type(t, &elem_type))
            return false;

         type_set_base(base, elem_type);

         return true;
      }

   case T_INTEGER:
   case T_PHYSICAL:
   case T_SUBTYPE:
      {
         range_t r = type_dim(type, 0);

         // Check the range expressions as if they were INTEGERs
         // when there is no base type
         type_set_push();
         type_set_add(type_kind(type) == T_SUBTYPE
                      ? base
                      : sem_std_type("STD.STANDARD.INTEGER"));
         bool ok = sem_check(r.left) && sem_check(r.right);
         type_set_pop();

         if (ok) {
            // Standard specifies type of 'LEFT and 'RIGHT are same
            // as the declared type
            tree_set_type(r.left, type);
            tree_set_type(r.right, type);
         }

         return ok;
      }

   default:
      return true;
   }
}

static bool sem_check_decl(tree_t t)
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
         sem_error(value, "type of initial value %s does not match type "
                   "of declaration %s", istr(type_ident(tree_type(value))),
                   istr(type_ident(type)));
   }

   if (tree_kind(t) == T_SIGNAL_DECL) {
      ident_t event_i = ident_new("EVENT");
      ident_t active_i = ident_new("ACTIVE");
      type_t std_bool = sem_std_type("STD.STANDARD.BOOLEAN");
      tree_add_attr_tree(t, event_i,
                         sem_builtin_fn(event_i, std_bool, "event"));
      tree_add_attr_tree(t, active_i,
                         sem_builtin_fn(active_i, std_bool, "active"));
   }

   scope_apply_prefix(t);
   return scope_insert(t);
}

static bool sem_check_func_decl(tree_t t)
{
   type_t ftype = tree_type(t);

   for (unsigned i = 0; i < tree_ports(t); i++) {
      tree_t p = tree_port(t, i);
      if (tree_port_mode(p) != PORT_IN)
         sem_error(p, "function arguments must have mode IN");

      type_t param_type = tree_type(p);
      if (!sem_check_type(p, &param_type))
         return false;

      type_add_param(ftype, param_type);
      tree_set_type(p, param_type);
   }

   type_t rtype = type_result(ftype);
   if (!sem_check_type(t, &rtype))
      return false;

   type_set_result(ftype, rtype);

   scope_apply_prefix(t);
   return scope_insert(t);
}

static bool sem_check_process(tree_t t)
{
   bool ok = true;

   scope_push(NULL);

   for (unsigned n = 0; n < tree_decls(t); n++)
      ok = sem_check(tree_decl(t, n)) && ok;

   if (ok) {
      for (unsigned n = 0; n < tree_stmts(t); n++)
         ok = sem_check(tree_stmt(t, n)) && ok;
   }

   scope_pop();

   return ok;
}

static bool sem_check_package(tree_t t)
{
   ident_t qual = ident_prefix(lib_name(lib_work()), tree_ident(t), '.');

   scope_push(qual);

   bool ok = sem_check_context(t);

   for (unsigned n = 0; n < tree_decls(t); n++)
      ok = sem_check(tree_decl(t, n)) && ok;

   scope_pop();

   tree_set_ident(t, qual);
   lib_put(lib_work(), t);

   return ok;
}

static bool sem_check_entity(tree_t t)
{
   scope_push(NULL);

   bool ok = sem_check_context(t);

   for (unsigned n = 0; n < tree_generics(t); n++)
      ok = sem_check(tree_generic(t, n)) && ok;

   for (unsigned n = 0; n < tree_ports(t); n++)
      ok = sem_check(tree_port(t, n)) && ok;

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

   scope_push(NULL);

   // Make all port and generic declarations available in this scope

   bool ok = sem_check_context(e);

   for (unsigned n = 0; n < tree_ports(e); n++)
      scope_insert(tree_port(e, n));

   for (unsigned n = 0; n < tree_generics(e); n++)
      scope_insert(tree_generic(e, n));

   // Now check the architecture itself

   ok = ok && sem_check_context(t);

   for (unsigned n = 0; n < tree_decls(t); n++)
      ok = sem_check(tree_decl(t, n)) && ok;

   if (ok) {
      for (unsigned n = 0; n < tree_stmts(t); n++)
         ok = sem_check(tree_stmt(t, n)) && ok;
   }

   scope_pop();

   // Prefix the architecture with the current library name
   ident_t lname = lib_name(lib_work());
   ident_t qual = ident_prefix(lname, tree_ident(t), '.');
   tree_set_ident(t, qual);
   ident_t ent_qual = ident_prefix(lname, tree_ident2(t), '.');
   tree_set_ident2(t, ent_qual);

   lib_put(lib_work(), t);

   return ok;
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

   tree_t decl = tree_ref(target);
   if (tree_kind(decl) != T_VAR_DECL)
      sem_error(target, "invalid target of variable assignment");

   if (!type_eq(tree_type(target), tree_type(value)))
      sem_error(t, "type of value %s does not match type of target %s",
                istr(type_ident(tree_type(value))),
                istr(type_ident(tree_type(target))));

   return true;
}

static bool sem_check_signal_assign(tree_t t)
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

   tree_t decl = tree_ref(target);
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

   if (!type_eq(tree_type(target), tree_type(value)))
      sem_error(t, "type of value %s does not match type of target %s",
                istr(type_ident(tree_type(value))),
                istr(type_ident(tree_type(target))));

   return true;
}

static void sem_maybe_ambiguous(tree_t t, void *_ambiguous)
{
   bool *ambiguous = _ambiguous;

   switch (tree_kind(t)) {
   case T_REF:
      {
         tree_t decl = scope_find(tree_ident(t));
         if (decl != NULL && tree_kind(decl) == T_ENUM_LIT)
            *ambiguous = true;
      }
      break;
   default:
      break;
   }
}

static bool sem_check_fcall(tree_t t)
{
   tree_t overloads[MAX_OVERLOADS];
   int n_overloads = 0;

   tree_t decl;
   int n = 0;
   do {
      if ((decl = scope_find_nth(tree_ident(t), n++))) {
         if (tree_kind(decl) != T_FUNC_DECL) {
            // The grammar is ambiguous between function calls and
            // array references so must be an array reference
            tree_change_kind(t, T_ARRAY_REF);
            return sem_check_array_ref(t);
         }

         type_t func_type = tree_type(decl);
         if (type_set_member(type_result(func_type))) {
            // Number of arguments must match
            if (type_params(func_type) != tree_params(t))
               continue;

            // Found a matching function definition
            overloads[n_overloads++] = decl;
         }
      }
   } while (decl != NULL);

   // Work out which parameters have ambiguous interpretations
   bool ambiguous[tree_params(t)];
   for (unsigned i = 0; i < tree_params(t); i++) {
      param_t p = tree_param(t, i);
      assert(p.kind == P_POS);
      ambiguous[i] = false;
      tree_visit(p.value, sem_maybe_ambiguous, &ambiguous[i]);
   }

   // First pass: only check those parameters which are unambiguous
   for (unsigned i = 0; i < tree_params(t); i++) {
      if (ambiguous[i])
         continue;

      type_set_push();

      for (int j = 0; j < n_overloads; j++) {
         if (overloads[j] != NULL)
            type_set_add(type_param(tree_type(overloads[j]), i));
      }

      param_t p = tree_param(t, i);
      assert(p.kind == P_POS);
      bool ok = sem_check(p.value);

      type_set_pop();

      if (ok) {
         // Delete all overloads which don't match this parameter type
         type_t ptype = tree_type(p.value);
         for (int j = 0; j < n_overloads; j++) {
            if (overloads[j] != NULL) {
               if (!type_eq(type_param(tree_type(overloads[j]), i),
                            ptype))
                  overloads[j] = NULL;
            }
         }
      }
      else
         return false;
   }

   // Second pass: now the set of overloads has been constrained check
   // those parameters which might be ambiguous
   for (unsigned i = 0; i < tree_params(t); i++) {
      if (!ambiguous[i])
         continue;

      type_set_push();

      for (int j = 0; j < n_overloads; j++) {
         if (overloads[j] != NULL)
            type_set_add(type_param(tree_type(overloads[j]), i));
      }

      param_t p = tree_param(t, i);
      assert(p.kind == P_POS);
      bool ok = sem_check(p.value);

      type_set_pop();

      if (!ok)
         return false;
   }

   int matches = 0;
   for (int n = 0; n < n_overloads; n++) {
      if (overloads[n] == NULL)
         continue;

      // Did argument types match for this overload?
      bool match = true;
      bool all_universal = true;
      type_t func_type = tree_type(overloads[n]);
      for (unsigned i = 0; i < tree_params(t); i++) {
         type_t ptype = tree_type(tree_param(t, i).value);
         match = match && type_eq(type_param(func_type, i), ptype);
         all_universal = all_universal && type_is_universal(ptype);
      }

      if (match) {
         bool builtin = tree_attr_str(overloads[n], ident_new("builtin"));
         if (all_universal && builtin) {
            // If all the arguments are universal integer or real and
            // this is a builtin function then it doesn't matter which
            // overload we pick as it will be constant-folded later
            type_t f_result = type_result(func_type);
            switch (type_kind(f_result)) {
            case T_INTEGER:
               tree_set_type(t, type_universal_int());
               break;
            default:
               tree_set_type(t, f_result);
            }
            tree_set_ref(t, overloads[n]);
            return true;
         }
         else {
            decl = overloads[n];
            matches++;
         }
      }
      else
         overloads[n] = NULL;
   }

   if (matches > 1) {
      char buf[1024];
      char *p = buf;
      const char *end = buf + sizeof(buf);
      const bool operator = !isalpha(*istr(tree_ident(t)));

      for (int n = 0; n < n_overloads; n++) {
         if (overloads[n] != NULL)
               p += snprintf(p, end - p, "    %s\n",
                             type_pp(tree_type(overloads[n])));
      }

      sem_error(t, "ambiguous %s %s\n%s",
                operator ? "use of operator" : "call to function",
                istr(tree_ident(t)), buf);
   }

   if (decl == NULL) {
      char fn[256];
      char *p = fn;
      const char *end = fn + sizeof(fn);
      const char *fname = istr(tree_ident(t));
      const bool operator = !isalpha(fname[0]);
      const char *quote = operator ? "\"" : "";

      p += snprintf(p, end - p, "%s%s%s(", quote, fname, quote);
      for (unsigned i = 0; i < tree_params(t); i++)
         p += snprintf(p, end - p, "%s%s",
                       (i == 0 ? "" : ", "),
                       istr(type_ident(tree_type(tree_param(t, i).value))));
      p += snprintf(p, end - p, ")");

      sem_error(t, (n == 1 ? "undefined %s %s"
                    : "no suitable overload for %s %s"),
                operator ? "operator" : "function",
                fn);
   }

#if 0
   printf("pick: %s\n", type_pp(tree_type(decl)));
   fmt_loc(stdout, tree_loc(t));
#endif

   tree_set_ref(t, decl);
   tree_set_type(t, type_result(tree_type(decl)));
   return true;
}

static bool sem_check_wait(tree_t t)
{
   if (tree_has_delay(t)) {
      tree_t delay = tree_delay(t);
      sem_check(delay);

      ident_t time_name = ident_new("STD.STANDARD.TIME");
      if (type_ident(tree_type(delay)) != time_name)
         sem_error(delay, "type of delay must be TIME");
   }

   return true;
}

static bool sem_check_assert(tree_t t)
{
   // Rules for asserion statements are in LRM 93 section 8.2

   type_t std_bool     = sem_std_type("STD.STANDARD.BOOLEAN");
   type_t std_string   = sem_std_type("STD.STANDARD.STRING");
   type_t std_severity = sem_std_type("STD.STANDARD.SEVERITY_LEVEL");

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
                "be %s but is %s", istr(type_ident(std_bool)),
                istr(type_ident(tree_type(value))));

   if (!type_eq(tree_type(severity), std_severity))
      sem_error(severity, "type of severity must be %s but is %s",
                istr(type_ident(std_severity)),
                istr(type_ident(tree_type(severity))));

   if (!type_eq(tree_type(message), std_string))
      sem_error(message, "type of message be %s but is %s",
                istr(type_ident(std_string)),
                istr(type_ident(tree_type(message))));

   return true;
}

static bool sem_check_literal(tree_t t)
{
   literal_t l = tree_literal(t);

   switch (l.kind) {
   case L_INT:
      tree_set_type(t, type_universal_int());
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

   type_t composite_type;
   if (!type_set_uniq(&composite_type))
      sem_error(t, "type of aggregate is ambiguous");

   // Aggregates are only valid for composite types

   switch (type_kind(composite_type)) {
   case T_CARRAY:
      break;
   case T_UARRAY:
      {
         // Create a new constrained array type with these dimensions

         type_t tmp = type_new(T_CARRAY);
         type_set_ident(tmp, type_ident(composite_type));
         type_set_base(tmp, type_base(composite_type));  // Element type

         // TODO: check index constraints

         range_t r = { .kind  = RANGE_TO,
                       .left  = sem_make_int(0),  // XXX: 'left of index type,
                       .right = sem_make_int(tree_assocs(t) - 1) };  // XXX
         type_add_dim(tmp, r);

         composite_type = tmp;
      }
      break;
   default:
      sem_error(t, "aggregates must have a composite type");
   }

   // All positional associations must appear before named associations
   // and those must appear before any others association

   enum { POS, NAMED, OTHERS } state = POS;

   for (unsigned i = 0; i < tree_assocs(t); i++) {
      assoc_t a = tree_assoc(t, i);

      switch (a.kind) {
      case A_POS:
         if (state > POS)
            sem_error(a.value, "positional associations must appear "
                      "first in aggregate");
         break;

      case A_NAMED:
      case A_RANGE:
         if (state > NAMED)
            sem_error(a.name, "named association must not follow "
                      "others association in aggregate");
         state = NAMED;
         break;

      case A_OTHERS:
         if (state == OTHERS)
            sem_error(a.value, "only a single others association "
                      "allowed in aggregate");
         state = OTHERS;
         break;
      }
   }

   // All elements must be of the composite base type

   type_t base = type_base(composite_type);

   for (unsigned i = 0; i < tree_assocs(t); i++) {
      assoc_t a = tree_assoc(t, i);

      if (!sem_check_constrained(a.value, base))
         return false;

      if (!type_eq(base, tree_type(a.value)))
         sem_error(a.value, "type of element %s does not match base "
                   "type of aggregate %s",
                   istr(type_ident(tree_type(a.value))),
                   istr(type_ident(base)));

   }

   tree_set_type(t, composite_type);
   return true;
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
                  && type_params(type) == 0
                  && type_set_member(type_result(type)))
            // Zero-argument function of correct type
            break;
         else if (!can_overload(decl))
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
      tree_set_type(t, tree_type(decl));
      break;

   case T_FUNC_DECL:
      tree_change_kind(t, T_FCALL);
      tree_set_type(t, type_result(tree_type(decl)));
      break;

   default:
      sem_error(t, "invalid use of %s", istr(tree_ident(t)));
   }

   tree_set_ref(t, decl);

   return true;
}

static bool sem_check_array_ref(tree_t t)
{
   tree_t value = tree_value(t);
   if (!sem_check(value))
      return false;

   type_t type = tree_type(value);
   if (type_kind(type) != T_CARRAY)
      sem_error(t, "invalid array reference");

   bool ok = true;
   for (unsigned i = 0; i < tree_params(t); i++) {
      param_t p = tree_param(t, i);
      if (p.kind != P_POS)
         sem_error(t, "only scalar references supported");

      // TODO: push type set containing index type of array
      ok = sem_check(p.value) && ok;
   }

   tree_set_type(t, type_base(type));
   tree_set_ref(t, tree_ref(value));
   return true;
}

static bool sem_check_array_slice(tree_t t)
{
   if (!sem_check(tree_value(t)))
      return false;

   type_t array_type = tree_type(tree_value(t));
   range_t r = type_dim(array_type, 0);

   type_set_push();
   type_set_add(sem_std_type("STD.STANDARD.INTEGER"));
   bool ok = sem_check(r.left) && sem_check(r.right);
   type_set_pop();

   if (!ok)
      return false;

   type_t slice_type = type_new(T_CARRAY);
   type_set_ident(slice_type, type_ident(array_type));
   type_set_base(slice_type, type_base(array_type));
   type_add_dim(slice_type, r);

   tree_set_ref(t, tree_ref(tree_value(t)));
   tree_set_type(t, slice_type);
   return true;
}

static bool sem_check_attr_ref(tree_t t)
{
   tree_t decl = scope_find(tree_ident(t));

   if (decl == NULL)
      sem_error(t, "undefined identifier %s", istr(tree_ident(t)));

   tree_t a = tree_attr_tree(decl, tree_ident2(t));
   if (a == NULL)
      sem_error(t, "%s has no attribute %s",
                istr(tree_ident(t)), istr(tree_ident2(t)));

   if (tree_kind(a) == T_FUNC_DECL) {
      tree_set_type(t, type_result(tree_type(a)));

      tree_t ref = tree_new(T_REF);
      tree_set_ident(ref, tree_ident(t));
      tree_set_loc(ref, tree_loc(t));
      tree_set_type(ref, tree_type(decl));
      tree_set_ref(ref, decl);

      param_t p = { .kind = P_POS, .value = ref };
      tree_add_param(t, p);
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

bool sem_check(tree_t t)
{
   switch (tree_kind(t)) {
   case T_ARCH:
      return sem_check_arch(t);
   case T_PACKAGE:
      return sem_check_package(t);
   case T_ENTITY:
      return sem_check_entity(t);
   case T_TYPE_DECL:
      return sem_check_type_decl(t);
   case T_SIGNAL_DECL:
   case T_VAR_DECL:
   case T_PORT_DECL:
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
   default:
      sem_error(t, "cannot check tree kind %d", tree_kind(t));
   }
}

int sem_errors(void)
{
   return errors;
}

void sem_bootstrap_en(bool en)
{
   bootstrap = en;
}

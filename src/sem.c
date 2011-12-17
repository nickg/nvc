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
   tree_t            subprog;

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

static bool sem_check_constrained(tree_t t, type_t type);
static bool sem_check_array_ref(tree_t t);
static bool sem_declare(tree_t decl);

static struct scope    *top_scope = NULL;
static int             errors = 0;
static struct type_set *top_type_set = NULL;
static bool            bootstrap = false;

#define sem_error(t, ...) {                           \
      error_at(t ? tree_loc(t) : NULL , __VA_ARGS__); \
      errors++;                                       \
      return false;                                   \
   }

static void scope_push(ident_t prefix)
{
   struct scope *s = xmalloc(sizeof(struct scope));
   s->decls    = NULL;
   s->prefix   = prefix;
   s->context  = NULL;
   s->imported = NULL;
   s->down     = top_scope;
   s->subprog  = NULL;

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

static void scope_replace_at(tree_t t, tree_t with, struct btree *where)
{
   assert(where != NULL);
   if (where->tree == t)
      where->tree = with;
   else {
      bool left = scope_btree_cmp(tree_ident(t), tree_ident(where->tree));

      struct btree *next = (left ? where->left : where->right);
      scope_replace_at(t, with, next);
   }
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

static void scope_replace(tree_t t, tree_t with)
{
   assert(top_scope != NULL);

   scope_replace_at(t, with, top_scope->decls);
}

static bool scope_import_unit(context_t ctx, lib_t lib)
{
   // Check we haven't already imported this
   struct ident_list *it;
   for (it = top_scope->imported; it != NULL; it = it->next) {
      if (it->ident == ctx.name)
         return true;
   }

   tree_t unit = lib_get(lib, ctx.name);
   if (unit == NULL) {
      error_at(&ctx.loc, "unit %s not found in library %s",
               istr(ctx.name), istr(lib_name(lib)));
      errors++;
      return false;
   }

   for (unsigned n = 0; n < tree_decls(unit); n++) {
      tree_t decl = tree_decl(unit, n);
      if (!sem_declare(decl))
         return false;
   }

   scope_ident_list_add(&top_scope->imported, ctx.name);
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

static tree_t sem_make_int(int i)
{
   literal_t l;
   l.kind = L_INT;
   l.i    = i;

   tree_t t = tree_new(T_LITERAL);
   tree_set_literal(t, l);
   tree_set_type(t, sem_std_type("STD.STANDARD.INTEGER"));

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

static tree_t sem_builtin_fn(ident_t name, type_t result,
                             const char *builtin, ...)
{
   type_t f = type_new(T_FUNC);
   type_set_ident(f, name);
   type_set_result(f, result);

   va_list ap;
   va_start(ap, builtin);
   type_t arg;
   while ((arg = va_arg(ap, type_t)))
      type_add_param(f, arg);
   va_end(ap);

   tree_t d = tree_new(T_FUNC_DECL);
   tree_set_ident(d, name);
   tree_set_type(d, f);
   tree_add_attr_str(d, ident_new("builtin"), builtin);

   return d;
}

static void sem_declare_binary(ident_t name, type_t lhs, type_t rhs,
                               type_t result, const char *builtin)
{
   tree_t d = sem_builtin_fn(name, result, builtin, NULL);
   type_add_param(tree_type(d), lhs);
   type_add_param(tree_type(d), rhs);

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
   case T_INCOMPLETE:
      assert(false);   // Shouldn't reach here

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

         tree_t image = sem_builtin_fn(ident_new("IMAGE"),
                                       sem_std_type("STD.STANDARD.STRING"),
                                       "image", t, NULL);
         tree_add_attr_tree(decl, ident_new("IMAGE"), image);
      }
      break;

   case T_ENUM:
      {
         tree_t left  = type_enum_literal(t, 0);
         tree_t right = type_enum_literal(t, type_enum_literals(t) - 1);
         tree_add_attr_tree(decl, ident_new("LEFT"), sem_make_ref(left));
         tree_add_attr_tree(decl, ident_new("RIGHT"), sem_make_ref(right));
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
            sem_error(t, "type %s is not defined", istr(type_ident(base)));

         base = tree_type(base_decl);
         type_set_base(type, base);
      }

      // If the subtype is not constrained then give it the same
      // range as its base type
      if (type_dims(type) == 0) {
         if (type_kind(base) == T_ENUM) {
            range_t r = {
               .kind  = RANGE_TO,
               .left  = sem_make_int(0),
               .right = sem_make_int(type_enum_literals(base) - 1)
            };
            type_add_dim(type, r);
         }
         else {
            for (unsigned i = 0; i < type_dims(base); i++)
               type_add_dim(type, type_dim(base, i));
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

   // Resolve the base type if necessary
   if (!sem_check_subtype(decl, tree_type(decl), NULL))
      return false;

   // If this is full type declarataion then replace any previous
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

   // Incomplete types cannot be checked any further
   if (type_kind(tree_type(decl)) == T_INCOMPLETE)
      return true;

   // Declare any predefined operators and attributes
   if (tree_kind(decl) == T_TYPE_DECL)
      sem_declare_predefined_ops(decl);

   bool ok = true;

   type_t type = tree_type(decl);
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

static bool sem_check_range(range_t *r)
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
      switch (type_kind(type)) {
      case T_CARRAY:
         *r = type_dim(type, 0);
         break;
      case T_ENUM:
      case T_UARRAY:
         {
            tree_t a = tree_new(T_ATTR_REF);
            tree_set_ident(a, tree_ident(r->left));
            tree_set_ident2(a, ident_new("LEFT"));

            tree_t b = tree_new(T_ATTR_REF);
            tree_set_ident(b, tree_ident(r->left));
            tree_set_ident2(b, ident_new("RIGHT"));

            r->kind  = RANGE_TO;
            r->left  = a;
            r->right = b;
         }
         break;
      default:
         sem_error(r->left, "%s does not have range",
                   istr(tree_ident(r->left)));
      }
   }

   if (!(sem_check(r->left) && sem_check(r->right)))
      return false;

   if (!type_eq(tree_type(r->left), tree_type(r->right)))
      sem_error(r->right, "type mismatch in range");

   return true;
}

static bool sem_check_context(tree_t t)
{
   ident_t work_name = lib_name(lib_work());

   // The work library should always be searched
   scope_add_context(work_name);

   // The std.standard package is also implicit unless we are
   // bootstrapping
   if (!bootstrap) {
      lib_t std = lib_find("std", true, true);
      if (std == NULL)
         fatal("failed to find std library");

      ident_t std_standard_name = ident_new("STD.STANDARD");
      scope_add_context(std_standard_name);

      context_t c = {
         .name = std_standard_name,
         .loc  = LOC_INVALID
      };
      if (!scope_import_unit(c, std))
         return false;
   }

   for (unsigned n = 0; n < tree_contexts(t); n++) {
      context_t c = tree_context(t, n);
      ident_t all = ident_strip(c.name, ident_new(".all"));
      if (all) {
         scope_add_context(all);
         c.name = all;
      }

      lib_t lib = lib_find(istr(ident_until(c.name, '.')), true, true);
      if (lib == NULL)
         return false;

      if (!scope_import_unit(c, lib))
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

static bool sem_check_array_dims(type_t type)
{
   for (unsigned i = 0; i < type_dims(type); i++) {
      range_t r = type_dim(type, i);

      // XXX: constrained by index type
      //      push type set first
      if (!sem_check_range(&r))
         return false;

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

               if (!sem_check_array_dims(*ptype))
                  return false;

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

static bool sem_check_resolution(type_t type)
{
   // Resolution functions are described in LRM 93 section 2.4

   assert(type_kind(type) == T_SUBTYPE);

   tree_t ref = type_resolution(type);

   tree_t fdecl = scope_find(tree_ident(ref));
   if (fdecl == NULL)
      sem_error(ref, "undefined resolution function %s",
                istr(tree_ident(ref)));

   if (tree_kind(fdecl) != T_FUNC_DECL)
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

   if (!type_eq(type_base(param), type))
      sem_error(fdecl, "parameter of resolution function must be "
                "array of %s", type_pp(type));

   // Return type must be the resolved type

   if (!type_eq(type_result(ftype), type))
      sem_error(fdecl, "result of resolution function must %s",
                type_pp(type));

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
         type_t elem_type = type_base(base);
         if (!sem_check_type(t, &elem_type))
            return false;

         type_set_base(base, elem_type);
      }
      break;
   default:
      break;
   }

   switch (type_kind(type)) {
   case T_CARRAY:
      return sem_check_array_dims(base);

   case T_UARRAY:
      for (unsigned i = 0; i < type_index_constrs(type); i++) {
         type_t index_type = type_index_constr(type, i);
         if (!sem_check_type(t, &index_type))
            return false;

         type_change_index_constr(type, i, index_type);
      }
      return true;

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

         if (type_kind(type) == T_SUBTYPE && type_has_resolution(type))
            ok = ok && sem_check_resolution(type);

         return ok;
      }

   default:
      return true;
   }
}

static void sem_add_attributes(tree_t decl)
{
   type_t std_bool = sem_std_type("STD.STANDARD.BOOLEAN");

   type_t type = tree_type(decl);
   type_kind_t kind = type_kind(type);

   if (kind == T_CARRAY) {
      range_t r = type_dim(type, 0);

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
   else if (kind == T_UARRAY) {
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
   }

   if (kind == T_UARRAY || kind == T_CARRAY) {
      ident_t length_i = ident_new("LENGTH");
      tree_add_attr_tree(decl, length_i,
                         sem_builtin_fn(length_i,
                                        sem_std_type("STD.STANDARD.INTEGER"),
                                        "length", type, NULL));
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

   tree_kind_t kind = tree_kind(t);
   if (kind == T_SIGNAL_DECL || kind == T_PORT_DECL) {
      ident_t event_i = ident_new("EVENT");
      ident_t active_i = ident_new("ACTIVE");
      ident_t last_value_i = ident_new("LAST_VALUE");
      type_t std_bool = sem_std_type("STD.STANDARD.BOOLEAN");
      tree_add_attr_tree(t, event_i,
                         sem_builtin_fn(event_i, std_bool, "event",
                                        type, NULL));
      tree_add_attr_tree(t, active_i,
                         sem_builtin_fn(active_i, std_bool, "active",
                                        type, NULL));
      tree_add_attr_tree(t, last_value_i,
                         sem_builtin_fn(last_value_i, type, "last_value",
                                        type, NULL));
   }

   sem_add_attributes(t);

   scope_apply_prefix(t);
   return scope_insert(t);
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

static bool sem_check_func_ports(tree_t t)
{
   type_t ftype = tree_type(t);

   for (unsigned i = 0; i < tree_ports(t); i++) {
      tree_t p = tree_port(t, i);
      if (tree_port_mode(p) != PORT_IN)
         sem_error(p, "function arguments must have mode IN");

      if (tree_class(p) == C_DEFAULT)
         tree_set_class(p, C_VARIABLE);

      type_t param_type = tree_type(p);
      if (!sem_check_type(p, &param_type))
         return false;

      type_add_param(ftype, param_type);
      tree_set_type(p, param_type);

      if (tree_has_value(p)) {
         tree_t value = tree_value(p);
         if (!sem_check_constrained(value, param_type))
            return false;

         if (!type_eq(tree_type(value), param_type))
            sem_error(value, "type of default value must be %s",
                      type_pp(param_type));
      }
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

         if (type_eq(tree_type(t), tree_type(decl)))
            break;
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

   scope_apply_prefix(t);

   // If there is no declaration for this function add to the scope
   if (!sem_check_duplicate(t, T_FUNC_DECL)) {
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

   unsigned nret = tree_visit_only(t, NULL, NULL, T_RETURN);
   if (nret == 0)
      sem_error(t, "function must contain a return statement");

   return true;
}

static bool sem_check_proc_ports(tree_t t)
{
   type_t ptype = tree_type(t);

   for (unsigned i = 0; i < tree_ports(t); i++) {
      tree_t p = tree_port(t, i);

      if (tree_class(p) == C_DEFAULT)
         tree_set_class(p, C_VARIABLE);

      type_t param_type = tree_type(p);
      if (!sem_check_type(p, &param_type))
         return false;

      type_add_param(ptype, param_type);
      tree_set_type(p, param_type);

      if (tree_has_value(p)) {
         tree_t value = tree_value(p);
         if (!sem_check_constrained(value, param_type))
            return false;

         if (!type_eq(tree_type(value), param_type))
            sem_error(value, "type of default value must be %s",
                      type_pp(param_type));
      }
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
   return true;
}

static bool sem_check_sensitivity(tree_t t)
{
   bool ok = true;
   for (unsigned i = 0; i < tree_triggers(t); i++) {
      tree_t r = tree_trigger(t, i);
      ok = sem_check(r) && sem_readable(r) && ok;

      if (ok) {
         // Can only reference signals in sensitivity list
         tree_t decl = tree_ref(r);
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

   scope_push(qual);

   if (!sem_check_context(t))
      return false;

   bool ok = true;
   for (unsigned n = 0; n < tree_decls(t); n++)
      ok = sem_check(tree_decl(t, n)) && ok;

   scope_pop();

   tree_set_ident(t, qual);
   lib_put(lib_work(), t);

   return ok;
}

static bool sem_check_package_body(tree_t t)
{
   ident_t qual = ident_prefix(lib_name(lib_work()), tree_ident(t), '.');

   scope_push(qual);

   if (!sem_check_context(t))
      return false;

   // Look up package declaration
   context_t c = {
      .name = qual,
      .loc  = *tree_loc(t)
   };
   bool ok = scope_import_unit(c, lib_work());

   for (unsigned n = 0; n < tree_decls(t); n++)
      ok = sem_check(tree_decl(t, n)) && ok;

   scope_pop();

   tree_set_ident(t, ident_prefix(qual, ident_new("body"), '-'));
   lib_put(lib_work(), t);

   return ok;
}

static bool sem_check_entity(tree_t t)
{
   scope_push(NULL);

   if (!sem_check_context(t))
      return false;

   bool ok = true;
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

   bool suitable = (tree_kind(decl) == T_VAR_DECL)
      || (tree_kind(decl) == T_PORT_DECL && tree_class(decl) == C_VARIABLE);

   if (!suitable)
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

   if (!sem_check(target))
      return false;

   type_t std_time = sem_std_type("STD.STANDARD.TIME");

   for (unsigned i = 0; i < tree_waveforms(t); i++) {
      tree_t waveform = tree_waveform(t, i);
      tree_t value = tree_value(waveform);

      if (!sem_check_constrained(value, tree_type(target)))
         return false;

      if (!sem_readable(value))
         return false;

      if (!type_eq(tree_type(target), tree_type(value)))
         sem_error(t, "type of value %s does not match type of target %s",
                   istr(type_ident(tree_type(value))),
                   istr(type_ident(tree_type(target))));

      if (tree_has_delay(waveform)) {
         tree_t delay = tree_delay(waveform);
         if (!sem_check(delay))
            return false;

         if (!type_eq(tree_type(delay), std_time))
            sem_error(delay, "type of delay must be %s",
                      type_pp(std_time));
      }
   }

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
   case T_AGGREGATE:
      *ambiguous = true;
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
         switch (tree_kind(decl)) {
         case T_FUNC_DECL:
         case T_FUNC_BODY:
            break;
         default:
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

   return sem_check_sensitivity(t);
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

   // All elements must be of the composite base type if this is
   // a one-dimensional array otherwise construct an array type
   // with n-1 dimensions.

   type_t base = NULL;
   if (type_dims(composite_type) == 1)
      base = type_base(composite_type);
   else {
      base = type_new(T_CARRAY);
      type_set_ident(base, type_ident(composite_type));
      type_set_base(base, type_base(composite_type));

      for (unsigned i = 1; i < type_dims(composite_type); i++)
         type_add_dim(base, type_dim(composite_type, i));
   }

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

   case T_ALIAS:
      tree_set_type(t, tree_type(decl));
      decl = tree_ref(tree_value(decl));
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

   unsigned nindex;
   type_t type = tree_type(value);
   switch (type_kind(type)) {
   case T_CARRAY:
      nindex = type_dims(type);
      break;
   case T_UARRAY:
      nindex = type_index_constrs(type);
      break;
   default:
      sem_error(t, "invalid array reference");
   }

   if (tree_params(t) != nindex)
      sem_error(t, "array %s has %d dimensions but %d indices given",
                istr(tree_ident(value)), nindex, tree_params(t));

   bool ok = true;
   for (unsigned i = 0; i < tree_params(t); i++) {
      param_t p = tree_param(t, i);
      if (p.kind != P_POS)
         sem_error(t, "only scalar references supported");

      type_t expect;
      if (type_kind(type) == T_CARRAY)
         expect = tree_type(type_dim(type, i).left);
      else
         expect = type_index_constr(type, i);

      ok = sem_check_constrained(p.value, expect) && ok;

      if (ok && !type_eq(expect, tree_type(p.value)))
         sem_error(p.value, "type of index %s does not match type of "
                   "array dimension %s",
                   istr(type_ident(tree_type(p.value))),
                   istr(type_ident(expect)));
   }

   tree_set_type(t, type_base(type));
   tree_set_ref(t, tree_ref(value));
   return ok;
}

static bool sem_check_array_slice(tree_t t)
{
   if (!sem_check(tree_value(t)))
      return false;

   type_t array_type = tree_type(tree_value(t));

   type_set_push();
   type_set_add(sem_std_type("STD.STANDARD.INTEGER"));
   range_t r = tree_range(t);
   bool ok = sem_check_range(&r);
   tree_set_range(t, r);
   type_set_pop();

   if (!ok)
      return false;

   type_t slice_type = type_new(T_CARRAY);
   type_set_ident(slice_type, type_ident(array_type));
   type_set_base(slice_type, type_base(array_type));
   type_add_dim(slice_type, tree_range(t));

   tree_set_ref(t, tree_ref(tree_value(t)));
   tree_set_type(t, slice_type);
   return true;
}

static bool sem_check_attr_ref(tree_t t)
{
   tree_t decl = scope_find(tree_ident(t));

   if (decl == NULL)
      sem_error(t, "undefined identifier %s", istr(tree_ident(t)));

   if (tree_ident2(t) == ident_new("range"))
      sem_error(t, "range expression not allowed here");

   tree_t a = tree_attr_tree(decl, tree_ident2(t));
   if (a == NULL)
      sem_error(t, "%s has no attribute %s",
                istr(tree_ident(t)), istr(tree_ident2(t)));

   if (tree_kind(a) == T_FUNC_DECL) {
      type_t ftype = tree_type(a);
      tree_set_type(t, type_result(ftype));

      if (tree_kind(decl) != T_TYPE_DECL) {
         // For an expression X'A add X as the final parameter
         tree_t ref = sem_make_ref(decl);
         tree_set_loc(ref, tree_loc(t));

         param_t p = { .kind = P_POS, .value = ref };
         tree_add_param(t, p);
      }

      if (tree_params(t) != type_params(ftype))
         sem_error(t, "expected %d parameters for attribute %s "
                   " but have %d", type_params(ftype),
                   istr(tree_ident2(t)), tree_params(t));

      for (unsigned i = 0; i < tree_params(t); i++) {
         param_t p = tree_param(t, i);
         if (p.kind != P_POS)
            sem_error(t, "only positional arguments supported here");

         type_t expect_type = type_param(ftype, i);
         if (!sem_check_constrained(p.value, expect_type))
            return false;

         if (!type_eq(tree_type(p.value), expect_type))
            sem_error(t, "expected type %s for attribute %s",
                      type_pp(expect_type), istr(tree_ident2(t)));
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

      case P_RANGE:
         sem_error(p.value, "ranges cannot be used here");
      }

      ok = sem_check_constrained(p.value, tree_type(decl)) && ok;
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
   // Find the referenced design unit
   tree_t unit = lib_get(lib_work(), tree_ident2(t));
   if (unit == NULL)
      sem_error(t, "cannot find unit %s", istr(tree_ident2(t)));

   tree_set_ref(t, unit);

   return sem_check_map(t, unit, tree_ports, tree_port,
                        tree_params, tree_param)
      && sem_check_map(t, unit, tree_generics, tree_generic,
                       tree_genmaps, tree_genmap);
}

static bool sem_check_if(tree_t t)
{
   type_t std_bool = sem_std_type("STD.STANDARD.BOOLEAN");

   tree_t value = tree_value(t);
   if (!sem_check_constrained(value, std_bool))
      return false;

   if (!type_eq(tree_type(value), std_bool))
      sem_error(value, "type of test must be %s but is %s",
                istr(type_ident(std_bool)),
                istr(type_ident(tree_type(value))));

   bool ok = true;
   for (unsigned i = 0; i < tree_stmts(t); i++)
      ok = sem_check(tree_stmt(t, i)) && ok;
   for (unsigned i = 0; i < tree_else_stmts(t); i++)
      ok = sem_check(tree_else_stmt(t, i)) && ok;

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
         sem_error(t, "expected return type %s", type_pp(expect));
   }

   return true;
}

static bool sem_check_while(tree_t t)
{
   type_t std_bool = sem_std_type("STD.STANDARD.BOOLEAN");

   tree_t value = tree_value(t);
   if (!sem_check_constrained(value, std_bool))
      return false;

   if (!type_eq(tree_type(value), std_bool))
      sem_error(value, "type of loop condition must be %s but is %s",
                istr(type_ident(std_bool)),
                istr(type_ident(tree_type(value))));

   bool ok = true;
   for (unsigned i = 0; i < tree_stmts(t); i++)
      ok = sem_check(tree_stmt(t, i)) && ok;

   return ok;
}

static bool sem_check_for(tree_t t)
{
   range_t r = tree_range(t);
   if (!sem_check_range(&r))
      return false;
   tree_set_range(t, r);

   type_t base = tree_type(tree_range(t).left);
   if (type_kind(base) == T_CARRAY)
      base = type_base(base);

   tree_t idecl = tree_new(T_VAR_DECL);
   tree_set_ident(idecl, tree_ident2(t));
   tree_set_loc(idecl, tree_loc(t));
   tree_set_type(idecl, base);

   tree_add_decl(t, idecl);

   scope_push(NULL);
   scope_insert(idecl);

   bool ok = true;
   for (unsigned i = 0; i < tree_stmts(t); i++)
      ok = sem_check(tree_stmt(t, i)) && ok;

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
      return sem_check_signal_assign(t);
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

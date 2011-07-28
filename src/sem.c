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

#include "sem.h"
#include "util.h"

#include <assert.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>

struct context {
   ident_t        prefix;
   struct context *next;
};

// TODO: replace with B-tree sorted by ident
#define MAX_VARS 16
struct scope {
   tree_t         decls[MAX_VARS];
   unsigned       n_decls;

   // For design unit scopes
   ident_t        prefix;
   struct context *context;
   
   struct scope   *down;
};

#define MAX_TS_MEMBERS 16
struct type_set {
   type_t          members[MAX_TS_MEMBERS];
   unsigned        n_members;

   struct type_set *down;
};

#define SEM_ERROR_SZ  1024

static void sem_def_error_fn(const char *msg, const loc_t *loc);

static struct scope    *top_scope = NULL;
static int             errors = 0;
static sem_error_fn_t  error_fn = sem_def_error_fn;
static struct type_set *top_type_set = NULL;

#define sem_error(t, ...) { _sem_error(t, __VA_ARGS__); return false; }

static void sem_def_error_fn(const char *msg, const loc_t *loc)
{
   fprintf(stderr, "%s:%d: %s\n", loc->file, loc->first_line, msg);
   fmt_loc(stderr, loc);
}

static void _sem_error(tree_t t, const char *fmt, ...)
{
   va_list ap;
   va_start(ap, fmt);

   char buf[SEM_ERROR_SZ];
   vsnprintf(buf, SEM_ERROR_SZ, fmt, ap);
   error_fn(buf, tree_loc(t));
   va_end(ap);

   errors++;
}

static void scope_push(ident_t prefix)
{
   struct scope *s = xmalloc(sizeof(struct scope));
   s->n_decls = 0;
   s->prefix  = prefix;
   s->context = NULL;
   s->down    = top_scope;
   
   top_scope = s;
}

static void scope_pop(void)
{
   assert(top_scope != NULL);

   struct context *it = top_scope->context;
   while (it != NULL) {
      struct context *next = it->next;
      free(it);
      it = next;
   }
   
   struct scope *s = top_scope;
   top_scope = s->down;
   free(s);
}

static void scope_add_context(ident_t prefix)
{
   assert(top_scope != NULL);

   struct context *c = xmalloc(sizeof(struct context));
   c->prefix = prefix;
   c->next   = top_scope->context;

   top_scope->context = c;
}

static void scope_apply_prefix(tree_t t)
{
   if (top_scope->prefix)
      tree_set_ident(t, ident_prefix(top_scope->prefix,
                                     tree_ident(t)));
}

static tree_t scope_find_in(ident_t i, struct scope *s, bool recur, int k)
{
   if (s == NULL)
      return NULL;
   else {
      for (unsigned n = 0; n < s->n_decls; n++) {
         tree_t d = s->decls[n];
         ident_t this = tree_ident(d);

         if (this == i
             || (s->prefix != NULL
                 && this == ident_prefix(s->prefix, i))) {
            if (k == 0)
               return d;
            else
               --k;
         }
         else {
            struct context *it;
            for (it = s->context; it != NULL; it = it->next) {
               if (this == ident_prefix(it->prefix, i)) {
                  if (k == 0)
                     return d;
                  else
                     --k;
               }
            }
         }
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
   return tree_kind(t) == T_ENUM_LIT;
}

static bool scope_insert(tree_t t)
{
   assert(top_scope != NULL);
   assert(top_scope->n_decls < MAX_VARS);

   if (!can_overload(t)
       && scope_find_in(tree_ident(t), top_scope, false, 0))
      sem_error(t, "%s already declared in this scope",
                istr(tree_ident(t)));   

   top_scope->decls[top_scope->n_decls++] = t;
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

static bool sem_check_context(tree_t t)
{
   // The work library should always be searched
   scope_add_context(lib_name(lib_work()));

   for (unsigned n = 0; n < tree_contexts(t); n++) {
      ident_t c = tree_context(t, n);
      ident_t all = ident_strip(c, ident_new(".all"));
      if (all) {
         scope_add_context(all);
         c = all;
      }

      tree_t unit = lib_get(lib_work(), c);
      if (unit == NULL)
         sem_error(t, "unit %s not found in library WORK", istr(c));

      // Import all declarations from t into the current scope
      
      for (unsigned n = 0; n < tree_decls(unit); n++)
         scope_insert(tree_decl(unit, n));
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
   
static bool sem_check_subtype(tree_t t, type_t type)
{
   while (type_kind(type) == T_SUBTYPE) {
      type_t base = type_base(type);
      tree_t base_decl = scope_find(type_ident(base));
      if (base_decl == NULL)
         sem_error(t, "type %s is not defined", istr(type_ident(base)));

      type_set_base(type, tree_type(base_decl));
      
      type = tree_type(base_decl);
   }

   return true;
}

static bool sem_check_type_decl(tree_t t)
{
   type_t type = tree_type(t);
   
   sem_check_subtype(t, type);   

   // Prefix the package name to the type name
   if (top_scope->prefix)
      type_set_ident(type, ident_prefix(top_scope->prefix,
                                        type_ident(type)));
   
   scope_apply_prefix(t);

   bool ok = scope_insert(t);
   if (ok && type_kind(type) == T_ENUM) {
      // Need to add each literal to the scope
      for (unsigned n = 0; n < type_enum_literals(type); n++)
         scope_insert(type_enum_literal(type, n));
   }

   return ok;
}

static bool sem_check_decl(tree_t t)
{
   type_t type = tree_type(t);
   switch (type_kind(type)) {
   case T_SUBTYPE:
      sem_check_subtype(t, type);
      break;

   case T_UNRESOLVED:
      {
         tree_t type_decl = scope_find(type_ident(type));
         if (type_decl == NULL)
            sem_error(t, "type %s is not defined", istr(type_ident(type)));
         
         tree_set_type(t, tree_type(type_decl));
      }
      break;

   default:
      assert(false);
   }

   if (tree_has_value(t))
      sem_check_constrained(tree_value(t), tree_type(t));

   scope_apply_prefix(t);
   return scope_insert(t);
}

static bool sem_check_process(tree_t t)
{
   bool ok = true;
   
   scope_push(NULL);

   for (unsigned n = 0; n < tree_decls(t); n++)
      ok = ok && sem_check(tree_decl(t, n));

   if (ok) {
      for (unsigned n = 0; n < tree_stmts(t); n++)
         ok = ok && sem_check(tree_stmt(t, n));
   }
   
   scope_pop();

   return ok;
}

static bool sem_check_package(tree_t t)
{
   ident_t qual = ident_prefix(lib_name(lib_work()), tree_ident(t));

   scope_push(qual);
 
   bool ok = sem_check_context(t);
  
   for (unsigned n = 0; n < tree_decls(t); n++)
      ok = ok && sem_check(tree_decl(t, n));

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
      ok = ok && sem_check(tree_generic(t, n));
   
   for (unsigned n = 0; n < tree_ports(t); n++)
      ok = ok && sem_check(tree_port(t, n));
   
   scope_pop();

   // Prefix the entity with the current library name
   ident_t qual = ident_prefix(lib_name(lib_work()), tree_ident(t));
   tree_set_ident(t, qual);
   lib_put(lib_work(), t);
   
   return ok;
}

static bool sem_check_arch(tree_t t)
{
   // Find the corresponding entity
   tree_t e = lib_get(lib_work(),
                      ident_prefix(lib_name(lib_work()),
                                   tree_ident2(t)));
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
      ok = ok && sem_check(tree_decl(t, n));

   if (ok) {
      // Keep going after failure in single statement
      int failures = 0;
      for (unsigned n = 0; n < tree_stmts(t); n++) {
         if (!sem_check(tree_stmt(t, n)))
            failures++;
      }

      ok = (failures == 0);
   }
   
   scope_pop();

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

static bool sem_check_wait(tree_t t)
{
   // TODO: need to check argument is time
   
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

static bool sem_check_ref(tree_t t)
{
   tree_t decl;
   int n = 0;
   do {
      if ((decl = scope_find_nth(tree_ident(t), n++))) {
         if (type_set_member(tree_type(decl)))
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
   case T_ENUM_LIT:
      break;
   default:
      sem_error(t, "invalid use of %s", istr(tree_ident(t)));
   }
   
   tree_set_type(t, tree_type(decl));
   tree_set_ref(t, decl);

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
      return sem_check_decl(t);
   case T_PROCESS:
      return sem_check_process(t);
   case T_VAR_ASSIGN:
      return sem_check_var_assign(t);
   case T_SIGNAL_ASSIGN:
      return sem_check_signal_assign(t);
   case T_LITERAL:
      return sem_check_literal(t);
   case T_REF:
      return sem_check_ref(t);
   case T_WAIT:
      return sem_check_wait(t);
   case T_QUALIFIED:
      return sem_check_qualified(t);
   default:
      sem_error(t, "cannot check tree kind %d", tree_kind(t));
   }
}

int sem_errors(void)
{
   return errors;
}

sem_error_fn_t sem_set_error_fn(sem_error_fn_t fn)
{
   sem_error_fn_t old = error_fn;
   error_fn = fn;
   return old;
}

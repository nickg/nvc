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

// TODO: replace with B-tree sorted by ident
#define MAX_VARS 16
struct scope {
   tree_t       decls[MAX_VARS];
   unsigned     n_decls;
   struct scope *down;
};

#define SEM_ERROR_SZ  1024

static void sem_def_error_fn(const char *msg, const loc_t *loc);

static struct scope   *top_scope = NULL;
static int            errors = 0;
static sem_error_fn_t error_fn = sem_def_error_fn;

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

static void scope_push(void)
{
   struct scope *s = xmalloc(sizeof(struct scope));
   s->n_decls = 0;
   s->down    = top_scope;
   
   top_scope = s;
}

static void scope_pop(void)
{
   assert(top_scope != NULL);

   struct scope *s = top_scope;
   top_scope = s->down;
   free(s);
}

static void scope_insert(tree_t t)
{
   assert(top_scope != NULL);
   assert(top_scope->n_decls < MAX_VARS);

   // TODO: check this name not in this scope already

   printf("scope_insert: %s\n", istr(tree_ident(t)));

   top_scope->decls[top_scope->n_decls++] = t;
}

static tree_t scope_find_in(ident_t i, struct scope *s)
{
   if (s == NULL)
      return NULL;
   else {
      for (unsigned n = 0; n < s->n_decls; n++) {
         if (tree_ident(s->decls[n]) == i)
            return s->decls[n];
      }

      return scope_find_in(i, s->down);
   }
}

static tree_t scope_find(ident_t i)
{
   return scope_find_in(i, top_scope);
}

static bool sem_check_subtype(tree_t t, type_t type)
{
   while (type_kind(type) == T_SUBTYPE) {
      type_t base = type_base(type);
      tree_t base_decl = scope_find(type_ident(base));
      if (base_decl == NULL)
         sem_error(t, "type %s is not defined", istr(type_ident(base)));

      printf("resolve %s to %s\n", istr(type_ident(type)),
             istr(type_ident(tree_type(base_decl))));
      type_set_base(type, tree_type(base_decl));
      
      type = tree_type(base_decl);
   }

   return true;
}

static bool sem_check_type_decl(tree_t t)
{
   sem_check_subtype(t, tree_type(t));   
   
   scope_insert(t);

   return true;
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
      sem_check(tree_value(t));

   scope_insert(t);

   return true;
}

static bool sem_check_process(tree_t t)
{
   bool ok = true;
   
   scope_push();

   for (unsigned n = 0; n < tree_decls(t); n++)
      ok = ok && sem_check(tree_decl(t, n));

   if (ok) {
      for (unsigned n = 0; n < tree_stmts(t); n++)
         ok = ok && sem_check(tree_stmt(t, n));
   }
   
   scope_pop();

   return ok;
}

static bool sem_check_arch(tree_t t)
{
   bool ok = true;
   
   // TODO: need to find entity and push its scope

   scope_push();

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
   
   bool ok = sem_check(target) && sem_check(value);
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
   tree_t decl = scope_find(tree_ident(t));
   if (decl == NULL)
      sem_error(t, "undefined identifier %s", istr(tree_ident(t)));

   assert(tree_kind(decl) == T_VAR_DECL
          || tree_kind(decl) == T_SIGNAL_DECL); // TODO: ...

   tree_set_type(t, tree_type(decl));
   tree_set_ref(t, decl);

   return true;
}

bool sem_check(tree_t t)
{
   switch (tree_kind(t)) {
   case T_ARCH:
      return sem_check_arch(t);
   case T_TYPE_DECL:
      return sem_check_type_decl(t);
   case T_SIGNAL_DECL:
   case T_VAR_DECL:
      return sem_check_decl(t);
   case T_PROCESS:
      return sem_check_process(t);
   case T_VAR_ASSIGN:
      return sem_check_var_assign(t);
   case T_LITERAL:
      return sem_check_literal(t);
   case T_REF:
      return sem_check_ref(t);
   default:
      assert(false);
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

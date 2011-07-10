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

static int errors = 0;

// TODO: replace with B-tree sorted by ident
#define MAX_VARS 16
struct scope {
   tree_t       decls[MAX_VARS];
   unsigned     n_decls;
   struct scope *down;
};

static struct scope *top_scope = NULL;

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
   printf("scope_find_in: %s %p\n", istr(i), s);
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

static void sem_check_type_decl(tree_t t)
{
   // TODO: various checks from the LRM...

   scope_insert(t);
}

static void sem_check_signal_decl(tree_t t)
{
   type_t type_name = tree_type(t);
   assert(type_kind(type_name) == T_UNRESOLVED);
      
   tree_t type_decl = scope_find(type_ident(type_name));
   assert(type_decl != NULL);
   // TODO: proper error message

   tree_set_type(t, tree_type(type_decl));

   scope_insert(t);
}

static void sem_check_arch(tree_t t)
{
   // TODO: need to find entity and push its scope

   scope_push();

   for (unsigned n = 0; n < tree_decls(t); n++)
      sem_check(tree_decl(t, n));

   scope_pop();
}

void sem_check(tree_t t)
{
   switch (tree_kind(t)) {
   case T_ARCH:
      sem_check_arch(t);
      break;
   case T_TYPE_DECL:
      sem_check_type_decl(t);
      break;
   case T_SIGNAL_DECL:
      sem_check_signal_decl(t);
      break;
   default:
      assert(false);
   }
}

int sem_errors(void)
{
   return errors;
}

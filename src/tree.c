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

#include "tree.h"
#include "util.h"

#include <assert.h>

struct tree {
   tree_kind_t kind;
   ident_t     ident;
   tree_list_t *ports;
};

#define IS(t, k) ((t)->kind == (k))
#define IS_DECL(t) (IS(t, T_PORT_DECL))
#define HAS_IDENT(t) (IS(t, T_ENTITY) || IS(t, T_PORT_DECL))
#define HAS_PORTS(t) (IS(t, T_ENTITY))

unsigned tree_list_len(struct tree_list *l)
{
   unsigned len = 0;
   while (l != NULL) {
      l = l->next;
      len++;
   }
   return len;
}

tree_t tree_list_nth(struct tree_list *l, unsigned n)
{
   while (n-- > 0) {
      assert(l != NULL);
      l = l->next;
   }
   assert(l != NULL);
   return l->value;
}

void tree_list_append(struct tree_list **l, tree_t t)
{
   struct tree_list *new = xmalloc(sizeof(struct tree_list));
   new->next  = NULL;
   new->value = t;

   tree_list_concat(l, new);
}

void tree_list_prepend(struct tree_list **l, tree_t t)
{
   struct tree_list *new = xmalloc(sizeof(struct tree_list));
   new->next  = *l;
   new->value = t;
   
   *l = new;
}

void tree_list_concat(struct tree_list **a, struct tree_list *b)
{
   if (*a == NULL)
      *a = b;
   else {
      struct tree_list *it;
      for (it = *a; it->next != NULL; it = it->next)
         ;
      it->next = b;
   }
}

tree_t tree_new(tree_kind_t kind)
{
   tree_t t = xmalloc(sizeof(struct tree));
   t->kind  = kind;
   t->ident = NULL;
   t->ports = NULL;
   
   return t;
}

ident_t tree_ident(tree_t t)
{
   assert(t != NULL);
   assert(HAS_IDENT(t));
   assert(t->ident != NULL);

   return t->ident;
}

void tree_set_ident(tree_t t, ident_t i)
{
   assert(t != NULL);
   assert(i != NULL);
   assert(HAS_IDENT(t));

   t->ident = i;
}

tree_kind_t tree_kind(tree_t t)
{
   assert(t != NULL);
   return t->kind;
}

unsigned tree_ports(tree_t t)
{
   assert(t != NULL);
   assert(HAS_PORTS(t));

   return tree_list_len(t->ports);
}

tree_t tree_port(tree_t t, unsigned n)
{
   assert(t != NULL);
   assert(HAS_PORTS(t));

   return tree_list_nth(t->ports, n);
}

void tree_add_port(tree_t t, tree_t d)
{
   assert(t != NULL);
   assert(d != NULL);
   assert(HAS_PORTS(t));
   assert(IS_DECL(d));

   tree_list_append(&t->ports, d);
}

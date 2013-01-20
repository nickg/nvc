//
//  Copyright (C) 2013  Nick Gasson
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

#include "util.h"
#include "common.h"

#include <assert.h>
#include <ctype.h>

int64_t assume_int(tree_t t)
{
   switch (tree_kind(t)) {
   case T_LITERAL:
      {
         literal_t l = tree_literal(t);
         assert(l.kind == L_INT);
         return l.i;
      }

   case T_REF:
      {
         tree_t ref = tree_ref(t);
         assert(tree_kind(ref) == T_ENUM_LIT);
         return tree_pos(ref);
      }

   default:
      fatal_at(tree_loc(t), "expression cannot be folded to "
               "an integer constant");
   }
}

void range_bounds(range_t r, int64_t *low, int64_t *high)
{
   int64_t left  = assume_int(r.left);
   int64_t right = assume_int(r.right);

   if (r.kind == RANGE_TO) {
      *low  = left;
      *high = right;
   }
   else {
      *low  = right;
      *high = left;
   }
}

tree_t call_builtin(const char *builtin, type_t type, ...)
{
   struct decl_cache {
      struct decl_cache *next;
      ident_t bname;
      tree_t  decl;
   };

   char name[64];
   snprintf(name, sizeof(name), "NVC.BUILTIN.%s", builtin);
   for (char *p = name; *p != '\0'; p++)
      *p = toupper((uint8_t)*p);

   static struct decl_cache *cache = NULL;

   ident_t bname = ident_new(builtin);
   ident_t name_i = ident_new(name);

   struct decl_cache *it;
   tree_t decl = NULL;
   for (it = cache; it != NULL; it = it->next) {
      if (it->bname == bname) {
         decl = it->decl;
         break;
      }
   }

   if (decl == NULL) {
      decl = tree_new(T_FUNC_DECL);
      tree_set_ident(decl, name_i);
      tree_add_attr_str(decl, ident_new("builtin"), ident_new(builtin));
   }

   struct decl_cache *c = xmalloc(sizeof(struct decl_cache));
   c->next  = cache;
   c->bname = bname;
   c->decl  = decl;

   cache = c;

   tree_t call = tree_new(T_FCALL);
   tree_set_ident(call, name_i);
   tree_set_ref(call, decl);
   if (type != NULL)
      tree_set_type(call, type);

   va_list ap;
   va_start(ap, type);
   tree_t arg;
   while ((arg = va_arg(ap, tree_t))) {
      param_t p = { .value = arg, { .pos = 0 }, .kind = P_POS };
      tree_add_param(call, p);
   }
   va_end(ap);

   return call;
}

bool folded_int(tree_t t, literal_t *l)
{
   if (tree_kind(t) == T_LITERAL) {
      *l = tree_literal(t);
      return (l->kind == L_INT);
   }
   else
      return false;
}

bool folded_real(tree_t t, literal_t *l)
{
   if (tree_kind(t) == T_LITERAL) {
      *l = tree_literal(t);
      return (l->kind == L_REAL);
   }
   else
      return false;
}

bool folded_bool(tree_t t, bool *b)
{
   ident_t std_bool_i = ident_new("STD.STANDARD.BOOLEAN");
   if (tree_kind(t) == T_REF) {
      tree_t decl = tree_ref(t);
      if (tree_kind(decl) == T_ENUM_LIT
          && type_ident(tree_type(decl)) == std_bool_i) {
         *b = (tree_pos(decl) == 1);
         return true;
      }
   }

   return false;
}

tree_t get_int_lit(tree_t t, int64_t i)
{
   literal_t l;
   l.kind = L_INT;
   l.i = i;

   tree_t f = tree_new(T_LITERAL);
   tree_set_loc(f, tree_loc(t));
   tree_set_literal(f, l);
   tree_set_type(f, tree_type(t));

   return f;
}

tree_t get_real_lit(tree_t t, double r)
{
   literal_t l;
   l.kind = L_REAL;
   l.r = r;

   tree_t f = tree_new(T_LITERAL);
   tree_set_loc(f, tree_loc(t));
   tree_set_literal(f, l);
   tree_set_type(f, tree_type(t));

   return f;
}

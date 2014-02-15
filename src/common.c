//
//  Copyright (C) 2013-2014  Nick Gasson
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
#include <string.h>
#include <stdlib.h>

static vhdl_standard_t current_std = STD_93;

int64_t assume_int(tree_t t)
{
   switch (tree_kind(t)) {
   case T_LITERAL:
      assert(tree_subkind(t) == L_INT);
      return tree_ival(t);

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

   char *name = xasprintf("NVC.BUILTIN.%s", builtin);
   for (char *p = name; *p != '\0'; p++)
      *p = toupper((int)*p);

   static struct decl_cache *cache = NULL;

   ident_t bname = ident_new(builtin);
   ident_t name_i = ident_new(name);
   free(name);

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
      tree_t p = tree_new(T_PARAM);
      tree_set_value(p, arg);
      tree_set_loc(p, tree_loc(arg));
      tree_set_subkind(p, P_POS);

      tree_add_param(call, p);
   }
   va_end(ap);

   return call;
}

bool folded_int(tree_t t, int64_t *l)
{
   if ((tree_kind(t) == T_LITERAL) && (tree_subkind(t) == L_INT)) {
      *l = tree_ival(t);
      return true;
   }
   else
      return false;
}

bool folded_real(tree_t t, double *l)
{
   if ((tree_kind(t) == T_LITERAL) && (tree_subkind(t) == L_REAL)) {
      *l = tree_dval(t);
      return true;
   }
   else
      return false;
}

bool folded_length(range_t r, int64_t *l)
{
   int64_t left, right;
   if (folded_int(r.left, &left) && folded_int(r.right, &right)) {
      if (r.kind == RANGE_TO)
         *l = MAX(right - left + 1, 0);
      else
         *l = MAX(left - right + 1, 0);
      return true;
   }
   else
      return false;
}

bool folded_bounds(range_t r, int64_t *low, int64_t *high)
{
   int64_t left, right;
   unsigned leftu, rightu;
   if (folded_int(r.left, &left) && folded_int(r.right, &right))
       ;
   else if (folded_enum(r.left, &leftu) && folded_enum(r.right, &rightu)) {
      left  = leftu;
      right = rightu;
   }
   else
      return false;

   switch (r.kind) {
   case RANGE_TO:
      *low  = left;
      *high = right;
      return true;
   case RANGE_DOWNTO:
      *low  = right;
      *high = left;
      return true;
   default:
      return false;
   }
}

bool folded_enum(tree_t t, unsigned *pos)
{
   if (tree_kind(t) == T_REF) {
      tree_t decl = tree_ref(t);
      if (tree_kind(decl) == T_ENUM_LIT) {
         *pos = tree_pos(decl);
         return true;
      }
   }

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
   tree_t f = tree_new(T_LITERAL);
   tree_set_loc(f, tree_loc(t));
   tree_set_subkind(f, L_INT);
   tree_set_ival(f, i);
   tree_set_type(f, tree_type(t));

   return f;
}

tree_t get_real_lit(tree_t t, double r)
{
   tree_t f = tree_new(T_LITERAL);
   tree_set_loc(f, tree_loc(t));
   tree_set_subkind(f, L_REAL);
   tree_set_dval(f, r);
   tree_set_type(f, tree_type(t));

   return f;
}

const char *package_signal_path_name(ident_t i)
{
   const char *str = istr(i);
   char *buf = get_fmt_buf(strlen(str) + 3);
   char *p = buf;

   *p++ = ':';
   while (*str != '\0') {
      if (*str == '.') {
         *p++ = ':';
         str++;
      }
      else {
         *p++ = tolower((int)*str);
         str++;
      }
   }
   *p = '\0';

   return buf;
}

bool parse_value(type_t type, const char *str, int64_t *value)
{
   while (isspace((int)*str))
      ++str;

   switch (type_kind(type_base_recur(type))) {
   case T_INTEGER:
      {
         int64_t sum = 0;
         while (isdigit((int)*str) || (*str == '_')) {
            if (*str != '_') {
               sum *= 10;
               sum += (*str - '0');
            }
            ++str;
         }

         *value = sum;
      }
      break;

   case T_ENUM:
      {
         bool upcase = true;
         char *copy = strdup(str), *p;
         for (p = copy; (*p != '\0') && !isspace((int)*p); p++, str++) {
            if (*p == '\'')
               upcase = false;
            if (upcase)
               *p = toupper((int)*p);
         }
         *p = '\0';

         ident_t id = ident_new(copy);
         free(copy);

         *value = -1;

         const int nlits = type_enum_literals(type);
         for (int i = 0; (*value == -1) && (i < nlits); i++) {
            if (tree_ident(type_enum_literal(type, i)) == id)
               *value = i;
         }

         if (*value == -1)
            return false;
      }
      break;

   default:
      break;
   }

   while (*str != '\0') {
      if (!isspace((int)*str)) {
         str++;
         return false;
      }
      else {
         str++;
      }
   }

   return true;
}

tree_t make_ref(tree_t to)
{
   tree_t t = tree_new(T_REF);
   tree_set_ident(t, tree_ident(to));
   tree_set_ref(t, to);
   tree_set_type(t, tree_type(to));
   return t;
}

vhdl_standard_t standard(void)
{
   return current_std;
}

void set_standard(vhdl_standard_t s)
{
   current_std = s;
}

const char *standard_text(vhdl_standard_t s)
{
   static const char *text[] = {
      "1987", "1993", "2000", "2002", "2008"
   };

   if ((unsigned)s < ARRAY_LEN(text))
      return text[s];
   else
      return "????";
}

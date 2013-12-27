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

#include "phase.h"
#include "util.h"

#include <limits.h>
#include <unistd.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <assert.h>

typedef enum {
   MAKE_TREE,
   MAKE_BC,
   MAKE_FINAL_BC,
   MAKE_LIB
} make_product_t;

static ident_t make_tag_i;

static lib_t make_get_lib(ident_t name)
{
   lib_t lib = lib_find(istr(ident_until(name, '.')), true, true);
   if (lib == NULL)
      fatal("cannot find library for %s", istr(name));
   else
      return lib;
}

static const char *make_product(tree_t t, make_product_t product)
{
   static char *cwd = NULL;
   if (cwd == NULL) {
      cwd = xmalloc(PATH_MAX);
      if (getcwd(cwd, PATH_MAX) == NULL)
         fatal_errno("getcwd");
   }

   char *buf = get_fmt_buf(PATH_MAX);

   ident_t name = tree_ident(t);

   const char *relpath = lib_path(make_get_lib(name));
   const char *a, *b;
   for (a = relpath, b = cwd;
        (*a != '\0') && (*b != '\0') && (*a == *b);
        a++, b++)
      ;

   if ((*b == '\0') && (*a == '/'))
      relpath = a + 1;

   switch (product) {
   case MAKE_TREE:
      checked_sprintf(buf, PATH_MAX, "%s/%s", relpath, istr(name));
      break;

   case MAKE_BC:
      checked_sprintf(buf, PATH_MAX, "%s/_%s.bc", relpath, istr(name));
      break;

   case MAKE_FINAL_BC:
      {
         ident_t base = ident_runtil(name, '.');
         checked_sprintf(buf, PATH_MAX, "%s/_%s.final.bc", relpath, istr(base));
      }
      break;

   case MAKE_LIB:
      checked_sprintf(buf, PATH_MAX, "%s", relpath);
      break;
   }

   return buf;
}

static void make_print_all_products(tree_t t, FILE *out)
{
   switch (tree_kind(t)) {
   case T_ELAB:
      fprintf(out, "%s %s %s",
              make_product(t, MAKE_TREE),
              make_product(t, MAKE_BC),
              make_product(t, MAKE_FINAL_BC));
      break;

   case T_PACKAGE:
   case T_PACK_BODY:
      fprintf(out, "%s ", make_product(t, MAKE_BC));
      // Fall-through

   case T_ENTITY:
   case T_ARCH:
      fprintf(out, "%s", make_product(t, MAKE_TREE));
      break;

   default:
      fatal("cannot print products for %s", tree_kind_str(tree_kind(t)));
   }
}

static void make_instance_deps(tree_t t, void *context)
{
   FILE *out = context;

   if (tree_class(t) == C_ENTITY) {
      ident_t name = tree_ident2(t);
      tree_t unit = lib_get(make_get_lib(name), name);
      if ((unit == NULL) || (tree_kind(unit) != T_ENTITY))
         warnf("cannot find entity %s", istr(name));
      else
         fprintf(out, " %s", make_product(unit, MAKE_TREE));
   }
}

static void make_rule(tree_t t, FILE *out)
{
   if (tree_attr_int(t, make_tag_i, 0))
      return;
   else
      tree_add_attr_int(t, make_tag_i, 1);

   lib_t work = make_get_lib(tree_ident(t));
   if (work != lib_work())
      return;

   make_print_all_products(t, out);
   fprintf(out, ":");

   const int nctx = tree_contexts(t);
   tree_t *deps = xmalloc(nctx * sizeof(tree_t));

   for (int i = 0; i < nctx; i++) {
      tree_t c = tree_context(t, i);
      ident_t name = tree_ident(c);

      deps[i] = lib_get(make_get_lib(name), name);
      if (deps[i] == NULL) {
         warnf("cannot find unit %s", istr(name));
         continue;
      }

      fprintf(out, " %s", make_product(deps[i], MAKE_TREE));
   }

   if (tree_kind(t) == T_ARCH)
      tree_visit_only(t, make_instance_deps, out, T_INSTANCE);

   switch (tree_kind(t)) {
   case T_ELAB:
      {
         const char *suffix = strchr(istr(tree_ident(t)), '.');
         assert(suffix != NULL);

         char *name = strdup(suffix + 1);
         for (char *p = name; *p != '\0'; p++) {
            if (*p == '.')
               *p = '\0';
            else
               *p = tolower(*p);
         }

         fprintf(out, "\n\tnvc -e %s\n", name);

         free(name);
      }
      break;

   case T_PACK_BODY:
      {
         ident_t pack_name = ident_until(tree_ident(t), '-');
         tree_t pack = lib_get(work, pack_name);
         if ((pack == NULL) || (tree_kind(pack) != T_PACKAGE))
            warnf("cannot find package %s", istr(pack_name));
         else
            fprintf(out, " %s\n", tree_loc(pack)->file);
      }
      // Fall-through

   case T_ENTITY:
   case T_ARCH:
   case T_PACKAGE:
      {
         const loc_t *loc = tree_loc(t);
         fprintf(out, " %s\n", loc->file);
         fprintf(out, "\tnvc -a %s\n", loc->file);
      }
      break;

   default:
      fatal("cannot generate rule for %s", tree_kind_str(tree_kind(t)));
   }

   fprintf(out, "\n");

   for (int i = 0; i < nctx; i++) {
      if (deps[i] != NULL)
         make_rule(deps[i], out);
   }

   free(deps);
}

static void make_header(tree_t *targets, int count, FILE *out)
{
   fprintf(out, "# Generated by " PACKAGE_STRING "\n\n");

   fprintf(out, "all: ");
   for (int i = 0; i < count; i++)
      make_print_all_products(targets[i], out);
   fprintf(out, "\n\n");
}

static void make_clean(tree_t dummy, FILE *out)
{
   fprintf(out, "clean:\n");
   fprintf(out, "\trm -r %s\n", make_product(dummy, MAKE_LIB));
}

void make(tree_t *targets, int count, FILE *out)
{
   make_tag_i = ident_new("make_tag");

   make_header(targets, count, out);

   for (int i = 0; i < count; i++)
      make_rule(targets[i], out);

   make_clean(targets[0], out);
}

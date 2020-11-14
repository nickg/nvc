//
//  Copyright (C) 2013-2018  Nick Gasson
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

#include "common.h"
#include "phase.h"
#include "util.h"
#include "loc.h"

#include <limits.h>
#include <unistd.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <assert.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>

typedef enum {
   MAKE_TREE,
   MAKE_LIB,
   MAKE_SO,
   MAKE_FINAL_SO,
   MAKE_IMPLIB,
   MAKE_VCODE,
} make_product_t;

typedef struct rule rule_t;

typedef enum {
   RULE_ANALYSE,
   RULE_ELABORATE
} rule_kind_t;

struct rule {
   rule_t       *next;
   rule_kind_t   kind;
   ident_list_t *outputs;
   ident_list_t *inputs;
   ident_t       source;
};

static ident_t make_tag_i;

static lib_t make_get_lib(ident_t name)
{
   return lib_find(ident_until(name, '.'), true);
}

static const char *make_product(tree_t t, make_product_t product)
{
   char *buf = get_fmt_buf(PATH_MAX);

   ident_t name = tree_ident(t);
   lib_t lib = make_get_lib(name);

   const char *path = lib_path(lib);

   switch (product) {
   case MAKE_TREE:
      checked_sprintf(buf, PATH_MAX, "%s/%s", path, istr(name));
      break;

   case MAKE_VCODE:
      checked_sprintf(buf, PATH_MAX, "%s/_%s.vcode", path, istr(name));
      break;

   case MAKE_SO:
      checked_sprintf(buf, PATH_MAX, "%s/_%s.so", path, istr(name));
      break;

   case MAKE_IMPLIB:
      checked_sprintf(buf, PATH_MAX, "%s/_%s.a", path, istr(name));
      break;

   case MAKE_FINAL_SO:
      {
         ident_t base = ident_runtil(name, '.');
         checked_sprintf(buf, PATH_MAX, "%s/_%s.final.so", path, istr(base));
      }
      break;

   case MAKE_LIB:
      checked_sprintf(buf, PATH_MAX, "%s", path);
      break;
   }

   return buf;
}

static void make_rule_add_input(rule_t *r, const char *input)
{
   ident_t ident = ident_new(input);

   for (ident_list_t *it = r->inputs; it != NULL; it = it->next) {
      if (it->ident == ident)
         return;
   }

   ident_list_add(&(r->inputs), ident);
}

static void make_rule_add_output(rule_t *r, const char *output)
{
   ident_t ident = ident_new(output);

   for (ident_list_t *it = r->outputs; it != NULL; it = it->next) {
      if (it->ident == ident)
         return;
   }

   ident_list_add(&(r->outputs), ident);
}

static rule_t *make_rule_for_source(rule_t **all, rule_kind_t kind,
                                    const char *source)
{
   ident_t ident = ident_new(source);

   for (rule_t *it = *all; it != NULL; it = it->next) {
      if (it->source == ident)
         return it;
   }

   rule_t *new = xmalloc(sizeof(rule_t));
   new->inputs  = NULL;
   new->outputs = NULL;
   new->kind    = kind;
   new->next    = *all;
   new->source  = ident;

   *all = new;
   return new;
}

static void make_free_rules(rule_t *list)
{
   while (list != NULL) {
      rule_t *tmp = list->next;
      ident_list_free(list->inputs);
      ident_list_free(list->outputs);
      free(list);
      list = tmp;
   }
}

static void make_instance_deps(tree_t t, void *context)
{
   rule_t *r = context;

   if (tree_class(t) == C_ENTITY) {
      ident_t name = tree_ident2(t);
      tree_t unit = lib_get(make_get_lib(name), name);
      if ((unit == NULL) || (tree_kind(unit) != T_ENTITY))
         warnf("cannot find entity %s", istr(name));
      else
         make_rule_add_input(r, make_product(unit, MAKE_TREE));
   }
}

static char *make_elab_name(tree_t t)
{
   const char *suffix = strchr(istr(tree_ident(t)), '.');
   assert(suffix != NULL);

   char *name = xstrdup(suffix + 1);
   for (char *p = name; *p != '\0'; p++) {
      if (*p == '.')
         *p = '\0';
      else
         *p = tolower((int)*p);
   }

   return name;
}

static void make_rule(tree_t t, rule_t **rules)
{
   if (tree_attr_int(t, make_tag_i, 0))
      return;
   else
      tree_add_attr_int(t, make_tag_i, 1);

   lib_t work = make_get_lib(tree_ident(t));
   if (work != lib_work())
      return;

   tree_kind_t kind = tree_kind(t);

   rule_t *r;
   if (kind == T_ELAB) {
      char *name = make_elab_name(t);
      r = make_rule_for_source(rules, RULE_ELABORATE, name);
      free(name);
   }
   else {
      const char *file = loc_file_str(tree_loc(t));
      r = make_rule_for_source(rules, RULE_ANALYSE, file);
      make_rule_add_input(r, file);

      if (kind == T_PACK_BODY) {
         ident_t pack_name = ident_until(tree_ident(t), '-');
         tree_t pack = lib_get(work, pack_name);
         if ((pack == NULL) || (tree_kind(pack) != T_PACKAGE))
            warnf("cannot find package %s", istr(pack_name));
         else
            make_rule_add_input(r, make_product(pack, MAKE_TREE));
      }
   }

   switch (kind) {
   case T_ELAB:
      make_rule_add_output(r, make_product(t, MAKE_TREE));
      make_rule_add_output(r, make_product(t, MAKE_FINAL_SO));
      break;

   case T_PACKAGE:
      if (pack_needs_cgen(t)) {
      case T_PACK_BODY:
         make_rule_add_output(r, make_product(t, MAKE_VCODE));
         make_rule_add_output(r, make_product(t, MAKE_SO));
         make_rule_add_output(r, make_product(t, MAKE_IMPLIB));
      }
      // Fall-through

   case T_ENTITY:
   case T_ARCH:
      make_rule_add_output(r, make_product(t, MAKE_TREE));
      break;

   default:
      fatal("cannot get products for %s", tree_kind_str(tree_kind(t)));
   }

   const int nctx = tree_contexts(t);
   tree_t *deps = xmalloc(nctx * sizeof(tree_t));

   const bool deps_only = opt_get_int("make-deps-only");

   for (int i = 0; i < nctx; i++) {
      tree_t c = tree_context(t, i);
      if (tree_kind(c) != T_USE) {
         deps[i] = NULL;
         continue;
      }

      ident_t name = tree_ident(c);
      lib_t lib = make_get_lib(name);

      deps[i] = lib_get(lib, name);
      if (deps[i] == NULL) {
         warnf("cannot find unit %s", istr(name));
         continue;
      }

      make_rule_add_input(r, make_product(deps[i], MAKE_TREE));

      if ((lib != work) && deps_only)
         deps[i] = NULL;
   }

   lib_t std = lib_find(std_i, false);
   if (std != NULL) {
      tree_t standard = lib_get(std, std_standard_i);
      if (standard)
         make_rule_add_input(r, make_product(standard, MAKE_TREE));
   }

   if (tree_kind(t) == T_ARCH)
      tree_visit_only(t, make_instance_deps, r, T_INSTANCE);

   for (int i = 0; i < nctx; i++) {
      if (deps[i] != NULL)
         make_rule(deps[i], rules);
   }

   free(deps);
}

static void make_header(tree_t *targets, int count, FILE *out)
{
   fprintf(out, "# Generated by " PACKAGE_STRING "\n\n");

   if (!opt_get_int("make-deps-only")) {
      fprintf(out, "all:");
      for (int i = 0; i < count; i++)
         fprintf(out, " %s", make_product(targets[i], MAKE_TREE));
      fprintf(out, "\n\n");
   }
}

static void make_clean(tree_t dummy, FILE *out)
{
   fprintf(out, "clean:\n");
   fprintf(out, "\trm -r %s\n", make_product(dummy, MAKE_LIB));
}

static void make_print_inputs(rule_t *r, FILE *out)
{
   for (ident_list_t *it = r->inputs; it != NULL; it = it->next) {
      bool circular = false;
      for (ident_list_t *o = r->outputs; o != NULL; o = o->next) {
         if (it->ident == o->ident)
            circular = true;
      }

      if (!circular)
         fprintf(out, " %s", istr(it->ident));
   }
}

static void make_print_rules(rule_t *rules, FILE *out)
{
   const bool deps_only = opt_get_int("make-deps-only");

   if (deps_only) {
      for (rule_t *r = rules; r != NULL; r = r->next) {
         for (ident_list_t *it = r->outputs; it != NULL; it = it->next) {
            fprintf(out, "%s:", istr(it->ident));
            make_print_inputs(r, out);
            fprintf(out, "\n\n");
         }
      }
   }
   else {
      for (rule_t *r = rules; r != NULL; r = r->next) {
         for (ident_list_t *it = r->outputs; it != NULL; it = it->next)
            fprintf(out, "%s%s", istr(it->ident),
                    (it->next == NULL) ? "" : " ");

         fprintf(out, ":");
         make_print_inputs(r, out);

         fprintf(out, "\n\tnvc ");

         switch (r->kind) {
         case RULE_ANALYSE:
            fprintf(out, "-a %s\n\n", istr(r->source));
            break;

         case RULE_ELABORATE:
            fprintf(out, "-e %s\n\n", istr(r->source));
            break;
         }
      }
   }
}

static void make_run(tree_t *targets, int count, FILE *out)
{
   int selected = -1;
   for (int i = 0; i < count; i++) {
      if (tree_kind(targets[i]) == T_ELAB) {
         if (selected != -1) {
            warnf("multiple elaborarted units found: %s is selected as "
                  "run target",
                  istr(ident_runtil(tree_ident(targets[selected]), '.')));
            return;
         }
         else {
            char *name LOCAL = make_elab_name(targets[i]);
            fprintf(out, "\nrun: all\n");
            fprintf(out, "\tnvc -r %s\n", name);
            fprintf(out, "\nwave: all\n");
            fprintf(out, "\tnvc -r -w %s\n", name);
            fprintf(out, "\n.PHONY: all run wave clean\n");
            selected = i;
         }
      }
   }
}

static void make_add_target(ident_t name, int kind, void *context)
{
   tree_t **outp = context;
   *(*outp)++ = lib_get(lib_work(), name);
}

void make(tree_t *targets, int count, FILE *out)
{
   make_tag_i = ident_new("make_tag");

   if (count == 0) {
      lib_t work = lib_work();
      count = lib_index_size(work);
      targets = xmalloc(count * sizeof(tree_t));
      tree_t *outp = targets;
      lib_walk_index(work, make_add_target, &outp);
   }

   make_header(targets, count, out);

   rule_t *rules = NULL;
   for (int i = 0; i < count; i++)
      make_rule(targets[i], &rules);

   make_print_rules(rules, out);
   make_free_rules(rules);

   if (!opt_get_int("make-deps-only")) {
      make_clean(targets[0], out);
      make_run(targets, count, out);
   }

   if (!opt_get_int("make-posix"))
      fprintf(out, "\n-include local.mk\n");
   else {
      struct stat dummy;
      if (stat("local.mk", &dummy) == 0)
         fprintf(out, "\ninclude local.mk\n");
   }

   free(targets);
}

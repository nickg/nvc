//
//  Copyright (C) 2011-2012  Nick Gasson
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

#include <ctype.h>
#include <assert.h>
#include <string.h>
#include <stdarg.h>

static void elab_arch(tree_t t, tree_t out, ident_t path);

static ident_t hpathf(ident_t path, char sep, const char *fmt, ...)
{
   va_list ap;
   char buf[256];
   va_start(ap, fmt);
   vsnprintf(buf, sizeof(buf), fmt, ap);
   va_end(ap);

   // LRM specifies instance path is lowercase
   char *p = buf;
   while (*p != '\0') {
      *p = tolower(*p);
      ++p;
   }

   return ident_prefix(path, ident_new(buf), sep);
}

static const char *simple_name(const char *full)
{
   // Strip off any library or entity prefix from the parameter
   const char *start = full;
   for (const char *p = full; *p != '\0'; p++) {
      if (*p == '.' || *p == '-')
         start = p + 1;
   }

   return start;
}

struct arch_search_params {
   ident_t name;
   tree_t  *arch;
};

static void find_arch(tree_t t, void *context)
{
   struct arch_search_params *params = context;

   if (tree_kind(t) == T_ARCH && tree_ident2(t) == params->name)
      *(params->arch) = t;
}

static tree_t pick_arch(ident_t ent_name)
{
   // XXX: LRM rules for selecting architecture?

   tree_t arch = NULL;
   struct arch_search_params params = { ent_name, &arch };
   lib_foreach(lib_work(), find_arch, &params);

   if (arch == NULL)
      fatal("no suitable architecture for entity %s", istr(ent_name));

   return arch;
}

struct rewrite_params {
   tree_t formal;
   tree_t actual;
};

static tree_t rewrite_ports(tree_t t, void *context)
{
   struct rewrite_params *params = context;

   switch (tree_kind(t)) {
   case T_REF:
      if (tree_kind(tree_ref(t)) == T_PORT_DECL
          && (tree_ident(t) == tree_ident(params->formal))) {

         switch (tree_kind(params->actual)) {
         case T_SIGNAL_DECL:
            tree_set_ref(t, params->actual);
            break;
         case T_LITERAL:
         case T_AGGREGATE:
            return params->actual;
         default:
            assert(false);
         }
      }
      break;

   default:
      break;
   }

   return t;
}

static void elab_add_alias(tree_t arch, tree_t decl, ident_t ident)
{
   tree_t a = tree_new(T_ALIAS);
   tree_set_ident(a, ident);
   tree_set_value(a, decl);
   tree_set_type(a, tree_type(decl));

   tree_add_decl(arch, a);
}

static void elab_copy_context(tree_t dest, tree_t src)
{
   for (unsigned i = 0; i < tree_contexts(src); i++)
      tree_add_context(dest, tree_context(src, i));
}

static void elab_map(tree_t t, tree_t arch,
                     tree_formals_t tree_Fs, tree_formal_t tree_F,
                     tree_actuals_t tree_As, tree_actual_t tree_A)
{
   tree_t unit = tree_ref(t);
   assert(tree_kind(unit) == T_ENTITY);

   // The map is processed in reverse order as generics may refer
   // to earlier ones which would have already been rewritten

   bool have_formals[tree_Fs(unit)];
   for (unsigned i = 0; i < tree_Fs(unit); i++)
      have_formals[i] = false;

   for (int i = tree_As(t) - 1; i >= 0; i--) {
      param_t p = tree_A(t, i);
      tree_t formal = NULL;

      switch (p.kind) {
      case P_POS:
         formal = tree_F(unit, p.pos);
         have_formals[p.pos] = true;
         break;
      case P_NAMED:
         for (unsigned j = 0; j < tree_Fs(unit); j++) {
            tree_t port = tree_F(unit, j);
            if (tree_ident(port) == p.name) {
               formal = port;
               have_formals[j] = true;
               break;
            }
         }
         break;
      default:
         assert(false);
      }
      assert(formal != NULL);

      struct rewrite_params params = {
         .formal = formal,
         .actual = tree_ref(p.value)
      };
      tree_rewrite(arch, rewrite_ports, &params);

      elab_add_alias(arch, p.value, tree_ident(formal));
   }

   for (int i = tree_Fs(unit) - 1; i >= 0; i--) {
      if (!have_formals[i]) {
         tree_t f = tree_F(unit, i);
         assert(tree_has_value(f));

         struct rewrite_params params = {
            .formal = f,
            .actual = tree_value(f)
         };
         tree_rewrite(arch, rewrite_ports, &params);
      }
   }
}

static void elab_instance(tree_t t, tree_t out, ident_t path)
{
   tree_t arch = tree_copy(pick_arch(tree_ident2(t)));

   elab_map(t, arch, tree_ports, tree_port,
            tree_params, tree_param);
   elab_map(t, arch, tree_generics, tree_generic,
            tree_genmaps, tree_genmap);

   elab_copy_context(out, tree_ref(t));
   elab_arch(arch, out, path);
}

static void elab_arch(tree_t t, tree_t out, ident_t path)
{
   elab_copy_context(out, t);

   for (unsigned i = 0; i < tree_decls(t); i++) {
      tree_t d = tree_decl(t, i);
      ident_t pn = hpathf(path, ':', "%s",
                          simple_name(istr(tree_ident(d))));

      switch (tree_kind(d)) {
      case T_SIGNAL_DECL:
      case T_FUNC_BODY:
      case T_ALIAS:
         tree_set_ident(d, pn);
         tree_add_decl(out, d);
         break;
      case T_FUNC_DECL:
         tree_set_ident(d, pn);
         break;
      case T_CONST_DECL:
         if (type_kind(tree_type(d)) == T_CARRAY) {
            tree_set_ident(d, pn);
            tree_add_decl(out, d);
         }
         break;
      default:
         break;
      }
   }

   for (unsigned i = 0; i < tree_stmts(t); i++) {
      tree_t s = tree_stmt(t, i);
      ident_t npath = hpathf(path, ':', "%s", istr(tree_ident(s)));
      tree_set_ident(s, npath);

      if (tree_kind(s) == T_INSTANCE)
         elab_instance(s, out, npath);
      else
         tree_add_stmt(out, s);
   }
}

static void elab_entity(tree_t t, tree_t out, ident_t path)
{
   if (tree_ports(t) > 0 || tree_generics(t) > 0) {
      // LRM 93 section 12.1 says implementation may allow this but
      // is not required to
      fatal("top-level entity may not have generics or ports");
   }

   tree_t arch = pick_arch(tree_ident(t));
   ident_t new_path = hpathf(path, ':', ":%s(%s)",
                             simple_name(istr(tree_ident(t))),
                             simple_name(istr(tree_ident(arch))));

   elab_copy_context(out, t);
   elab_arch(arch, out, new_path);
}

tree_t elab(tree_t top)
{
   lib_load_all(lib_work());

   tree_t e = tree_new(T_ELAB);
   tree_set_ident(e, ident_prefix(tree_ident(top),
                                  ident_new("elab"), '.'));

   switch (tree_kind(top)) {
   case T_ENTITY:
      elab_entity(top, e, NULL);
      break;
   default:
      fatal("%s is not a suitable top-level unit", istr(tree_ident(top)));
   }

   simplify(e);
   if (simplify_errors() == 0) {
      lib_put(lib_work(), e);
      return e;
   }
   else
      return NULL;
}

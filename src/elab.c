//
//  Copyright (C) 2011-2013  Nick Gasson
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
#include "common.h"

#include <ctype.h>
#include <assert.h>
#include <string.h>
#include <stdarg.h>
#include <inttypes.h>

#define FUNC_REPLACE_MAX 32

typedef struct {
   tree_t   out;
   ident_t  path;    // Current 'PATH_NAME
   ident_t  inst;    // Current 'INSTANCE_NAME
   netid_t *next_net;
} elab_ctx_t;

typedef struct {
   tree_t formal;
   tree_t actual;
} rewrite_params_t;

static void elab_arch(tree_t t, const elab_ctx_t *ctx);
static void elab_block(tree_t t, const elab_ctx_t *ctx);
static void elab_stmts(tree_t t, const elab_ctx_t *ctx);

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
      *p = tolower((uint8_t)*p);
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

static void find_arch(ident_t name, int kind, void *context)
{
   struct arch_search_params *params = context;

   lib_t work = lib_work();

   ident_t prefix = ident_until(name, '-');

   if ((kind == T_ARCH) && (prefix == params->name)) {
      tree_t t = lib_get(work, name);

      if (*(params->arch) == NULL)
         *(params->arch) = t;
      else {
         lib_mtime_t old_mtime = lib_mtime(work, tree_ident2(*(params->arch)));
         lib_mtime_t new_mtime = lib_mtime(work, tree_ident2(t));

         if (new_mtime == old_mtime) {
            // Analysed at the same time: compare line number
            // Note this assumes both architectures are from the same
            // file but this shouldn't be a problem with high-resolution
            // timestamps
            uint16_t new_line = tree_loc(t)->first_line;
            uint16_t old_line = tree_loc(*(params->arch))->first_line;

            if (new_line > old_line)
               *(params->arch) = t;
         }
         else if (new_mtime > old_mtime)
            *(params->arch) = t;
      }
   }
}

static tree_t pick_arch(const loc_t *loc, ident_t name)
{
   // When an explicit architecture name is not given select the most
   // recently analysed architecture of this entity

   tree_t arch = lib_get(lib_work(), name);
   if ((arch == NULL) || (tree_kind(arch) != T_ARCH)) {
      arch = NULL;
      struct arch_search_params params = { name, &arch };
      lib_walk_index(lib_work(), find_arch, &params);

      if (arch == NULL)
         fatal_at(loc, "no suitable architecture for %s", istr(name));
   }

   return arch;
}

static tree_t rewrite_refs(tree_t t, void *context)
{
   rewrite_params_t *params = context;

   if (tree_kind(t) != T_REF)
      return t;

   tree_t decl = tree_ref(t);

   tree_kind_t decl_kind = tree_kind(decl);
   if (decl_kind != tree_kind(params->formal))
      return t;

   const bool match =
      (decl == params->formal)
      || ((decl_kind == T_PORT_DECL)
          && (tree_ident(t) == tree_ident(params->formal)));

   if (!match)
      return t;

   // Delete assignments to OPEN ports
   if (params->actual == NULL)
      return NULL;

   switch (tree_kind(params->actual)) {
   case T_SIGNAL_DECL:
   case T_ENUM_LIT:
      tree_set_ref(t, params->actual);
      break;
   case T_LITERAL:
   case T_AGGREGATE:
   case T_REF:
      return params->actual;
   default:
      fatal_trace("cannot handle tree kind %s in rewrite_refs",
                  tree_kind_str(tree_kind(t)));
   }

   return t;
}

static tree_t elab_port_to_signal(tree_t arch, tree_t port)
{
   assert(tree_kind(port) == T_PORT_DECL);

   tree_t s = tree_new(T_SIGNAL_DECL);
   tree_set_ident(s, tree_ident(port));
   tree_set_type(s, tree_type(port));

   tree_add_decl(arch, s);
   return s;
}

static void elab_copy_context(tree_t dest, tree_t src)
{
   for (unsigned i = 0; i < tree_contexts(src); i++)
      tree_add_context(dest, tree_context(src, i));
}

static unsigned elab_signal_width(tree_t decl)
{
   assert(tree_kind(decl) == T_SIGNAL_DECL);

   type_t type = tree_type(decl);
   if (type_is_array(type)) {
      int64_t low, high;
      range_bounds(type_dim(type, 0), &low, &high);
      return high - low + 1;
   }
   else
      return 1;
}

static tree_t elab_signal_port(tree_t arch, tree_t formal, tree_t actual)
{
   switch (tree_kind(actual)) {
   case T_REF:
      {
         // Replace the formal port with a signal and connect its nets to
         // those of the actual
         tree_t s   = elab_port_to_signal(arch, formal);
         tree_t ref = tree_ref(actual);

         const int width = elab_signal_width(s);
         assert(width == elab_signal_width(ref));
         assert(tree_nets(ref) == width);

         for (int i = 0; i < width; i++) {
            printf("map %s[%d] -> %s[%d] net %d\n", istr(tree_ident(s)), i,
                   istr(tree_ident(ref)), i, tree_net(ref, i));
            tree_add_net(s, tree_net(ref, i));
         }

         return s;
      }

   case T_LITERAL:
      return actual;

   case T_OPEN:
      return NULL;

   default:
      fatal_at(tree_loc(actual), "tree %s not supported as actual",
               tree_kind_str(tree_kind(actual)));
   }
}

static void elab_map(tree_t t, tree_t arch,
                     tree_formals_t tree_Fs, tree_formal_t tree_F,
                     tree_actuals_t tree_As, tree_actual_t tree_A)
{
   tree_t unit = tree_ref(t);
   assert(tree_kind(unit) == T_ENTITY);

   const int nformals = tree_Fs(unit);
   const int nactuals = tree_As(t);

   bool have_formals[nformals];
   for (int i = 0; i < nformals; i++)
      have_formals[i] = false;

   for (int i = 0; i < nactuals; i++) {
      tree_t p = tree_A(t, i);
      tree_t formal = NULL;

      switch (tree_subkind(p)) {
      case P_POS:
         {
            const int pos = tree_pos(p);
            formal = tree_F(unit, pos);
            have_formals[pos] = true;
         }
         break;
      case P_NAMED:
         {
            tree_t name = tree_name(p);
            assert(tree_kind(name) == T_REF);
            for (int j = 0; j < nformals; j++) {
               tree_t port = tree_F(unit, j);
               if (tree_ident(port) == tree_ident(name)) {
                  formal = port;
                  have_formals[j] = true;
                  break;
               }
            }
         }
         break;
      default:
         assert(false);
      }
      assert(formal != NULL);

      rewrite_params_t params = {
         .formal = formal,
         .actual = NULL
      };

      switch (tree_class(formal)) {
      case C_SIGNAL:
         params.actual = elab_signal_port(arch, formal, tree_value(p));
         break;

      case C_CONSTANT:
         params.actual = tree_value(p);
         break;

      default:
         assert(false);
      }

      tree_rewrite(arch, rewrite_refs, &params);
   }

   // Assign default values
   for (unsigned i = 0; i < nformals; i++) {
      if (!have_formals[i]) {
         tree_t f = tree_F(unit, i);
         assert(tree_has_value(f));

         rewrite_params_t params = {
            .formal = f,
            .actual = tree_value(f)
         };
         tree_rewrite(arch, rewrite_refs, &params);
      }
   }
}

static void elab_instance(tree_t t, elab_ctx_t *ctx)
{
   // Default binding indication is described in LRM 93 section 5.2.2

   if ((tree_class(t) == C_COMPONENT) || (tree_class(t) == C_CONFIGURATION))
      fatal_at(tree_loc(t), "sorry, instantiating components or configurations "
               "is not supported yet");

   tree_t arch = tree_copy(pick_arch(tree_loc(t), tree_ident2(t)));

   elab_map(t, arch, tree_ports, tree_port,
            tree_params, tree_param);
   elab_map(t, arch, tree_generics, tree_generic,
            tree_genmaps, tree_genmap);

   elab_copy_context(ctx->out, tree_ref(t));

   ident_t ninst = hpathf(ctx->inst, '@', "%s(%s)",
                          simple_name(istr(tree_ident2(arch))),
                          simple_name(istr(tree_ident(arch))));

   simplify(arch);

   elab_ctx_t new_ctx = {
      .out      = ctx->out,
      .path     = ctx->path,
      .inst     = ninst,
      .next_net = ctx->next_net
   };
   elab_arch(arch, &new_ctx);
}

static void elab_nets(tree_t decl, const elab_ctx_t *ctx)
{
   // Assign net IDs to each sub-element of a signal declaration

   if (tree_nets(decl) != 0) {
      // Nets have already been assigned e.g. from a port map
   }
   else {
      const int width = elab_signal_width(decl);
      for (int i = 0; i < width; i++) {
         printf("assign %s[%d] net %d\n", istr(tree_ident(decl)),
                i, *(ctx->next_net));
         tree_add_net(decl, (*ctx->next_net)++);
      }
   }
}

static void elab_decls(tree_t t, const elab_ctx_t *ctx)
{
   ident_t inst_name_i = ident_new("INSTANCE_NAME");

   const int ndecls = tree_decls(t);
   for (int i = 0; i < ndecls; i++) {
      tree_t d = tree_decl(t, i);
      const char *label = simple_name(istr(tree_ident(d)));
      ident_t ninst = hpathf(ctx->inst, ':', "%s", label);
      ident_t npath = hpathf(ctx->path, ':', "%s", label);

      switch (tree_kind(d)) {
      case T_SIGNAL_DECL:
         elab_nets(d, ctx);
         // Fall-through
      case T_FUNC_BODY:
      case T_PROC_BODY:
      case T_ALIAS:
      case T_FILE_DECL:
      case T_VAR_DECL:
         tree_set_ident(d, npath);
         tree_add_decl(ctx->out, d);
         tree_add_attr_str(d, inst_name_i, ninst);
         break;
      case T_FUNC_DECL:
      case T_PROC_DECL:
         tree_set_ident(d, npath);
         break;
      case T_CONST_DECL:
         if (type_is_array(tree_type(d))) {
            tree_set_ident(d, npath);
            tree_add_attr_str(d, inst_name_i, ninst);
            tree_add_decl(ctx->out, d);
         }
         break;
      default:
         break;
      }
   }
}

static void elab_for_generate(tree_t t, elab_ctx_t *ctx)
{
   int64_t low, high;
   range_bounds(tree_range(t), &low, &high);

   for (int64_t i = low; i <= high; i++) {
      tree_t copy = tree_copy(t);

      tree_t genvar = tree_ref(copy);
      rewrite_params_t params = {
         .formal = genvar,
         .actual = get_int_lit(genvar, i)
      };
      tree_rewrite(copy, rewrite_refs, &params);

      //const char *label = istr(tree_ident(copy));
      ident_t npath = hpathf(ctx->path, '\0', "[%"PRIi64"]", i);
      ident_t ninst = hpathf(ctx->inst, '\0', "[%"PRIi64"]", i);

      elab_ctx_t new_ctx = {
         .out      = ctx->out,
         .path     = npath,
         .inst     = ninst,
         .next_net = ctx->next_net
      };

      elab_decls(copy, &new_ctx);
      elab_stmts(copy, &new_ctx);
   }
}

static void elab_stmts(tree_t t, const elab_ctx_t *ctx)
{
   const int nstmts = tree_stmts(t);
   for (int i = 0; i < nstmts; i++) {
      tree_t s = tree_stmt(t, i);
      const char *label = istr(tree_ident(s));
      ident_t npath = hpathf(ctx->path, ':', "%s", label);
      ident_t ninst = hpathf(ctx->inst, ':', "%s", label);

      elab_ctx_t new_ctx = {
         .out      = ctx->out,
         .path     = npath,
         .inst     = ninst,
         .next_net = ctx->next_net
      };

      switch (tree_kind(s)) {
      case T_INSTANCE:
         elab_instance(s, &new_ctx);
         break;
      case T_BLOCK:
         elab_block(s, &new_ctx);
         break;
      case T_FOR_GENERATE:
         elab_for_generate(s, &new_ctx);
         break;
      case T_IF_GENERATE:
         fatal("IF-GENERATE statement was not constant folded");
      default:
         tree_add_stmt(ctx->out, s);
      }

      tree_set_ident(s, npath);
   }
}

static void elab_block(tree_t t, const elab_ctx_t *ctx)
{
   elab_decls(t, ctx);
   elab_stmts(t, ctx);
}

static void elab_arch(tree_t t, const elab_ctx_t *ctx)
{
   elab_copy_context(ctx->out, t);
   elab_decls(t, ctx);
   elab_stmts(t, ctx);
}

static void elab_entity(tree_t t, const elab_ctx_t *ctx)
{
   if (tree_ports(t) > 0 || tree_generics(t) > 0) {
      // LRM 93 section 12.1 says implementation may allow this but
      // is not required to
      fatal("top-level entity may not have generics or ports");
   }

   tree_t arch = pick_arch(NULL, tree_ident(t));
   const char *name = simple_name(istr(tree_ident(t)));
   ident_t ninst = hpathf(ctx->inst, ':', ":%s(%s)", name,
                          simple_name(istr(tree_ident(arch))));
   ident_t npath = hpathf(ctx->path, ':', ":%s", name);

   elab_copy_context(ctx->out, t);

   elab_ctx_t new_ctx = {
      .out      = ctx->out,
      .path     = npath,
      .inst     = ninst,
      .next_net = ctx->next_net
   };
   elab_arch(arch, &new_ctx);
}

static tree_t rewrite_funcs(tree_t t, void *context)
{
   if (tree_kind(t) != T_FCALL)
      return t;

   tree_t decl = tree_ref(t);

   ident_t name = tree_ident(decl);
   for (tree_t *rlist = context; *rlist != NULL; ++rlist) {
      const bool match =
         (tree_ident(*rlist) == name)
         && (type_eq(tree_type(*rlist), tree_type(decl)));

      if (match) {
         tree_set_ref(t, *rlist);
         break;
      }
   }

   return t;
}

static void elab_funcs(tree_t t)
{
   // Look through all the package bodies required by the context and
   // rewrite references to function declarations with references to
   // the function body.  This allows these functions to be folded by
   // the simplify phase

   int nreplace = 0;
   tree_t rlist[FUNC_REPLACE_MAX];

   const int ncontext = tree_contexts(t);
   for (int i = 0; i < ncontext; i++) {
      context_t ctx = tree_context(t, i);

      lib_t lib = lib_find(istr(ident_until(ctx.name, '.')), true, true);
      if (lib == NULL)
         fatal("cannot link library %s", istr(ctx.name));

      ident_t body_i = ident_prefix(ctx.name, ident_new("body"), '-');
      tree_t body = lib_get(lib, body_i);
      if (body == NULL)
         continue;

      const int ndecls = tree_decls(body);
      for (int j = 0; j < ndecls; j++) {
         tree_t decl = tree_decl(body, j);
         if (tree_kind(decl) != T_FUNC_BODY)
            continue;

         if (nreplace + 1 == FUNC_REPLACE_MAX) {
            tree_rewrite(t, rewrite_funcs, rlist);
            nreplace = 0;
         }

         rlist[nreplace++] = decl;
         rlist[nreplace] = NULL;
      }
   }

   if (nreplace > 0)
      tree_rewrite(t, rewrite_funcs, rlist);
}

tree_t elab(tree_t top)
{
   tree_t e = tree_new(T_ELAB);
   tree_set_ident(e, ident_prefix(tree_ident(top),
                                  ident_new("elab"), '.'));

   netid_t next_net = 0;
   elab_ctx_t ctx = {
      .out      = e,
      .path     = NULL,
      .inst     = NULL,
      .next_net = &next_net
   };

   switch (tree_kind(top)) {
   case T_ENTITY:
      elab_entity(top, &ctx);
      break;
   default:
      fatal("%s is not a suitable top-level unit", istr(tree_ident(top)));
   }

   elab_funcs(e);

   tree_add_attr_int(e, ident_new("nnets"), next_net);

   if (simplify_errors() == 0) {
      lib_put(lib_work(), e);
      return e;
   }
   else
      return NULL;
}

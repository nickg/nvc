//
//  Copyright (C) 2011-2022  Nick Gasson
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
#include "array.h"
#include "common.h"
#include "diag.h"
#include "eval.h"
#include "hash.h"
#include "lib.h"
#include "opt.h"
#include "phase.h"
#include "type.h"

#include <ctype.h>
#include <assert.h>
#include <stdarg.h>
#include <stdlib.h>
#include <inttypes.h>

typedef A(tree_t) tree_list_t;
typedef A(type_t) type_list_t;

typedef struct {
   tree_t       out;
   tree_t       root;
   ident_t      path;           // Current 'PATH_NAME
   ident_t      inst;           // Current 'INSTANCE_NAME
   ident_t      dotted;
   ident_t      prefix[2];
   lib_t        library;
   hash_t      *generics;
   hash_t      *subprograms;
   eval_t      *eval;
   tree_list_t  enames;
} elab_ctx_t;

typedef struct {
   tree_list_t copied_subs;
   type_list_t copied_types;
   tree_list_t external_names;
} elab_copy_ctx_t;

typedef struct {
   lib_t    lib;
   ident_t  name;
   tree_t   comp;
   tree_t  *tree;
} lib_search_params_t;

typedef struct generic_list generic_list_t;

struct generic_list {
   generic_list_t *next;
   ident_t         name;
   char           *value;
   bool            used;
};

static void elab_arch(tree_t t, const elab_ctx_t *ctx);
static void elab_block(tree_t t, const elab_ctx_t *ctx);
static void elab_stmts(tree_t t, const elab_ctx_t *ctx);
static void elab_decls(tree_t t, const elab_ctx_t *ctx);
static void elab_push_scope(tree_t t, elab_ctx_t *ctx);
static void elab_pop_scope(elab_ctx_t *ctx);
static void elab_block_config(tree_t config, const elab_ctx_t *ctx);
static tree_t elab_copy(tree_t t, elab_ctx_t *ctx);

static generic_list_t *generic_override = NULL;

static ident_t hpathf(ident_t path, char sep, const char *fmt, ...)
{
   va_list ap;
   va_start(ap, fmt);
   char *buf = xvasprintf(fmt, ap);
   va_end(ap);

   // LRM specifies instance path is lowercase
   char *p = buf;
   while (*p != '\0') {
      *p = tolower((int)*p);
      ++p;
   }

   ident_t id = ident_new(buf);
   free(buf);
   return ident_prefix(path, id, sep);
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

static lib_t elab_find_lib(ident_t name, const elab_ctx_t *ctx)
{
   ident_t lib_name = ident_until(name, '.');
   if (lib_name == well_known(W_WORK))
      return ctx->library;
   else
      return lib_require(lib_name);
}

static void elab_find_arch_cb(lib_t lib, ident_t name, int kind, void *context)
{
   lib_search_params_t *params = context;

   ident_t prefix = ident_until(name, '-');

   if ((kind == T_ARCH) && (prefix == params->name)) {
      bool error;
      tree_t t = lib_get_check_stale(params->lib, name, &error);
      assert(t != NULL);
      assert(!error);

      if (*(params->tree) == NULL)
         *(params->tree) = t;
      else {
         lib_mtime_t old_mtime = lib_mtime(params->lib,
                                           tree_ident(*(params->tree)));
         lib_mtime_t new_mtime = lib_mtime(params->lib, tree_ident(t));

         if (new_mtime == old_mtime) {
            // Analysed at the same time: compare line number
            // Note this assumes both architectures are from the same
            // file but this shouldn't be a problem with high-resolution
            // timestamps
            uint16_t new_line = tree_loc(t)->first_line;
            uint16_t old_line = tree_loc(*(params->tree))->first_line;

            if (new_line > old_line)
               *(params->tree) = t;
         }
         else if (new_mtime > old_mtime)
            *(params->tree) = t;
      }
   }
}

static tree_t elab_pick_arch(const loc_t *loc, tree_t entity,
                             const elab_ctx_t *ctx)
{
   // When an explicit architecture name is not given select the most
   // recently analysed architecture of this entity

   ident_t name = tree_ident(entity);
   lib_t lib = elab_find_lib(name, ctx);
   ident_t search_name =
      ident_prefix(lib_name(lib), ident_rfrom(name, '.'), '.');

   tree_t arch = NULL;
   lib_search_params_t params = { lib, search_name, NULL, &arch };
   lib_walk_index(lib, elab_find_arch_cb, &params);

   if (arch == NULL)
      fatal_at(loc, "no suitable architecture for %s", istr(search_name));

   return arch;
}

static bool elab_should_copy_tree(tree_t t, void *__ctx)
{
   switch (tree_kind(t)) {
   case T_INSTANCE:
   case T_EXTERNAL_NAME:
      return true;
   case T_FUNC_DECL:
   case T_PROC_DECL:
      return !(tree_flags(t) & TREE_F_PREDEFINED);
   case T_FUNC_BODY:
   case T_PROC_BODY:
   case T_FUNC_INST:
   case T_PROC_INST:
      return true;
   case T_PATH_ELT:
      return tree_subkind(t) != PE_SIMPLE;
   case T_FCALL:
      // Globally static expressions should be copied and folded
      return !!(tree_flags(t) & TREE_F_GLOBALLY_STATIC);
   case T_REF:
      {
         tree_t decl = tree_ref(t);
         switch (tree_kind(decl)) {
         case T_GENERIC_DECL:
         case T_PORT_DECL:
            return true;
         case T_ENTITY:
         case T_ARCH:
         case T_BLOCK:
            // These may appear in attribute references like 'PATH_NAME
            // which need to get rewritten to point at the corresponding
            // block in the elaborated design
            return true;
         default:
            return false;
         }
      }
   case T_VAR_DECL:
      return !!(tree_flags(t) & TREE_F_SHARED);
   default:
      return false;
   }
}

static bool elab_should_copy_type(type_t type, void *__ctx)
{
   return type_kind(type) == T_GENERIC;
}

static void elab_tree_copy_cb(tree_t t, void *__ctx)
{
   elab_copy_ctx_t *ctx = __ctx;

   if (is_subprogram(t))
      APUSH(ctx->copied_subs, t);
   else if (tree_kind(t) == T_EXTERNAL_NAME)
      APUSH(ctx->external_names, t);
}

static void elab_type_copy_cb(type_t type, void *__ctx)
{
   elab_copy_ctx_t *ctx = __ctx;

   if (type_has_ident(type))
      APUSH(ctx->copied_types, type);
}

static void elab_find_config_roots(tree_t t, tree_list_t *roots)
{
   switch (tree_kind(t)) {
   case T_BLOCK_CONFIG:
      {
         tree_t what = tree_ref(t);
         if (tree_kind(what) == T_ARCH) {
            APUSH(*roots, tree_primary(what));
            APUSH(*roots, what);
         }
      }
      // Fall-through

   case T_SPEC:
      {
         const int ndecls = tree_decls(t);
         for (int i = 0; i < ndecls; i++)
            elab_find_config_roots(tree_decl(t, i), roots);
      }
      break;

   default:
      break;
   }
}

static tree_t elab_copy(tree_t t, elab_ctx_t *ctx)
{
   elab_copy_ctx_t copy_ctx = {};

   type_copy_pred_t type_pred = NULL;
   if (standard() >= STD_08)
      type_pred = elab_should_copy_type;

   tree_list_t roots = AINIT;
   switch (tree_kind(t)) {
   case T_ARCH:
      APUSH(roots, tree_primary(t));
      break;
   case T_CONFIGURATION:
      elab_find_config_roots(tree_decl(t, 0), &roots);
      break;
   default:
      fatal_trace("unexpected %s in elab_copy", tree_kind_str(tree_kind(t)));
   }
   APUSH(roots, t);    // Architecture must be processed last

   tree_copy(roots.items, roots.count, elab_should_copy_tree, type_pred,
             elab_tree_copy_cb, elab_type_copy_cb, &copy_ctx);

   tree_t copy = roots.items[roots.count - 1];
   ACLEAR(roots);

   // Change the name of any copied types to reflect the new hiearchy
   for (unsigned i = 0; i < copy_ctx.copied_types.count; i++) {
      type_t type = copy_ctx.copied_types.items[i];
      ident_t orig = type_ident(type);
      for (unsigned j = 0; j < ARRAY_LEN(ctx->prefix); j++) {
         if (ident_starts_with(orig, ctx->prefix[j])) {
            LOCAL_TEXT_BUF tb = tb_new();
            tb_cat(tb, istr(ctx->dotted));
            tb_cat(tb, istr(orig) + ident_len(ctx->prefix[j]));

            type_set_ident(type, ident_new(tb_get(tb)));
            break;
         }
      }
   }
   ACLEAR(copy_ctx.copied_types);

   // Change the mangled name of copied subprograms so that copies in
   // different instances do not collide
   for (unsigned i = 0; i < copy_ctx.copied_subs.count; i++) {
      tree_t decl = copy_ctx.copied_subs.items[i];
      if (tree_kind(decl) == T_GENERIC_DECL)
         continue;   // Does not yet have mangled name

      ident_t orig = tree_ident2(decl);
      for (unsigned j = 0; j < ARRAY_LEN(ctx->prefix); j++) {
         if (ident_starts_with(orig, ctx->prefix[j])) {
            ident_t prefix = ident_runtil(orig, '(');

            LOCAL_TEXT_BUF tb = tb_new();
            tb_cat(tb, istr(ctx->dotted));
            tb_cat(tb, istr(prefix) + ident_len(ctx->prefix[j]));

            const tree_kind_t kind = tree_kind(decl);
            const bool is_func = kind == T_FUNC_BODY || kind == T_FUNC_DECL;
            const int nports = tree_ports(decl);
            if (nports > 0 || is_func)
               tb_append(tb, '(');

            for (int k = 0; k < nports; k++) {
               tree_t p = tree_port(decl, k);
               if (tree_class(p) == C_SIGNAL)
                  tb_printf(tb, "s");
               mangle_one_type(tb, tree_type(p));
            }

            if (nports > 0 || is_func)
               tb_printf(tb, ")");

            if (is_func)
               mangle_one_type(tb, type_result(tree_type(decl)));

            ident_t mangled = ident_new(tb_get(tb));
            tree_set_ident2(decl, mangled);

            // Save a reference to this subprogram so we can find it
            // later if we need to lower on-demand during simplification
            const bool may_need_to_lower =
               kind == T_FUNC_BODY || kind == T_PROC_BODY
               || kind == T_FUNC_INST || kind == T_PROC_INST
               || (kind == T_FUNC_DECL && tree_subkind(decl) != S_USER);

            if (may_need_to_lower)
               hash_put(ctx->subprograms, mangled, decl);

            break;
         }
      }
   }
   ACLEAR(copy_ctx.copied_subs);

   ctx->enames = copy_ctx.external_names;

   return copy;
}

static void elab_subprogram_prefix(tree_t arch, elab_ctx_t *ctx)
{
   // Get the prefix of unit that will need to be rewritten in
   // subprogram names

   assert(tree_kind(arch) == T_ARCH);

   // The order is important here because the architecture name is
   // prefixed with the entity
   ctx->prefix[0] = tree_ident(arch);
   ctx->prefix[1] = tree_ident(tree_primary(arch));
}

static void elab_config_instance(tree_t block, tree_t spec,
                                 const elab_ctx_t *ctx)
{
   ident_t match = tree_has_ident(spec) ? tree_ident(spec) : NULL;

   const int nstmts = tree_stmts(block);
   for (int i = 0; i < nstmts; i++) {
      tree_t s = tree_stmt(block, i);
      if (tree_kind(s) != T_INSTANCE)
         continue;

      const bool apply = match == well_known(W_ALL)
         || tree_ident(s) == match
         || (match == NULL && !tree_has_spec(s));

      if (apply) tree_set_spec(s, spec);
   }

   const int ndecls = tree_decls(spec);
   for (int i = 0; i < ndecls; i++)
      elab_block_config(tree_decl(spec, i), ctx);
}

static void elab_block_config(tree_t config, const elab_ctx_t *ctx)
{
   assert(tree_kind(config) == T_BLOCK_CONFIG);

   tree_t what = tree_ref(config);

   const int ndecls = tree_decls(config);
   for (int i = 0; i < ndecls; i++) {
      tree_t d = tree_decl(config, i);
      switch (tree_kind(d)) {
      case T_SPEC:
         elab_config_instance(what, d, ctx);
         break;
      case T_BLOCK_CONFIG:
         elab_block_config(d, ctx);
         break;
      default:
         fatal_trace("cannot handle block config item %s",
                     tree_kind_str(tree_kind(d)));
      }
   }
}

static tree_t elab_root_config(tree_t top, elab_ctx_t *ctx)
{
   tree_t copy = elab_copy(top, ctx);

   tree_t config = tree_decl(copy, 0);
   assert(tree_kind(config) == T_BLOCK_CONFIG);

   tree_t arch = tree_ref(config);
   assert(tree_kind(arch) == T_ARCH);

   const int ndecls = tree_decls(config);
   for (int i = 0; i < ndecls; i++) {
      tree_t d = tree_decl(config, i);
      switch (tree_kind(d)) {
      case T_SPEC:
         elab_config_instance(arch, d, ctx);
         break;
      case T_BLOCK_CONFIG:
         elab_block_config(d, ctx);
         break;
      default:
         fatal_trace("cannot handle block config item %s",
                     tree_kind_str(tree_kind(d)));
      }
   }

   return arch;
}

static bool elab_compatible_map(tree_t comp, tree_t entity, char *what,
                                tree_t inst, tree_formals_t tree_Fs,
                                tree_formal_t tree_F)
{
   const int comp_nf   = (*tree_Fs)(comp);
   const int entity_nf = (*tree_Fs)(entity);

   for (int i = 0; i < comp_nf; i++) {
      tree_t comp_f = (*tree_F)(comp, i);

      bool found = false;
      for (int j = 0; !found && j < entity_nf; j++) {
         tree_t entity_f = (*tree_F)(entity, j);

         if (tree_ident(comp_f) != tree_ident(entity_f))
            continue;

         found = true;

         type_t entity_type = tree_type(entity_f);
         type_t comp_type   = tree_type(comp_f);

         if (!type_eq(entity_type, comp_type)) {
            diag_t *d = diag_new(DIAG_ERROR, tree_loc(comp_f));
            diag_printf(d, "type of %s %s in component declaration %s is "
                        "%s which does not match type %s in entity %s",
                        what, istr(tree_ident(comp_f)),
                        istr(tree_ident(comp)), type_pp(comp_type),
                        type_pp(entity_type), istr(tree_ident(entity)));
            diag_hint(d, tree_loc(comp), "in component declaration of %s",
                      istr(tree_ident(comp)));
            diag_hint(d, tree_loc(comp_f), "port declared as %s",
                      type_pp(comp_type));
            diag_hint(d, tree_loc(entity_f), "entity port declared as %s",
                      type_pp(entity_type));
            diag_hint(d, tree_loc(inst), "while elaborating instance %s here",
                      istr(tree_ident(inst)));
            diag_emit(d);
            return false;
         }
      }

      if (!found) {
         diag_t *d = diag_new(DIAG_ERROR, tree_loc(comp_f));
         diag_printf(d, "%s %s not found in entity %s",
                     what, istr(tree_ident(comp_f)), istr(tree_ident(entity)));
         diag_hint(d, tree_loc(comp), "in component declaration of %s",
                      istr(tree_ident(comp)));
         diag_hint(d, tree_loc(comp_f), "declaration of port %s",
                   istr(tree_ident(comp_f)));
         diag_hint(d, tree_loc(inst), "while elaborating instance %s here",
                   istr(tree_ident(inst)));
         diag_hint(d, tree_loc(entity), "entity %s has no port named %s",
                   istr(tree_ident(entity)), istr(tree_ident(comp_f)));
         diag_emit(d);
         return false;
      }
   }

   return true;
}

static void elab_find_entity_cb(lib_t lib, ident_t name, int kind, void *__ctx)
{
   lib_search_params_t *params = __ctx;

   if (kind == T_ENTITY && params->name == name) {
      bool error;
      *(params->tree) = lib_get_check_stale(params->lib, name, &error);
      assert(!error);
   }
}

static bool elab_synth_binding_cb(lib_t lib, void *__ctx)
{
   lib_search_params_t *params = __ctx;

   params->lib  = lib;
   params->name = ident_prefix(lib_name(lib), tree_ident(params->comp), '.');

   lib_walk_index(lib, elab_find_entity_cb, params);

   return *(params->tree) == NULL;
}

static tree_t elab_default_binding(tree_t inst, const elab_ctx_t *ctx)
{
   // Default binding indication is described in LRM 93 section 5.2.2

   tree_t comp = tree_ref(inst);

   ident_t full_i = tree_ident(comp);
   ident_t lib_i = ident_until(full_i, '.');

   lib_t lib = NULL;
   bool synth_binding = true;
   if (lib_i == full_i) {
      lib    = ctx->library;
      full_i = ident_prefix(lib_name(lib), full_i, '.');
   }
   else {
      synth_binding = false;
      lib = elab_find_lib(lib_i, ctx);

      // Strip out the component package name, if any
      full_i = ident_prefix(lib_i, ident_rfrom(full_i, '.'), '.');
   }

   tree_t entity = NULL;
   lib_search_params_t params = { lib, full_i, comp, &entity };
   lib_walk_index(params.lib, elab_find_entity_cb, &params);

   if (entity == NULL && synth_binding) {
      // This is not correct according to the LRM but matches the
      // behaviour of many synthesis tools
      lib_for_all(elab_synth_binding_cb, &params);
   }

   if (entity == NULL) {
      error_at(tree_loc(inst), "cannot find entity for component %s "
               "without binding indication", istr(tree_ident(comp)));
      return NULL;
   }

   tree_t arch = elab_pick_arch(tree_loc(comp), entity, ctx);

   // Check entity is compatible with component declaration

   if (!elab_compatible_map(comp, entity, "generic", inst,
                            tree_generics, tree_generic))
      return NULL;

   if (!elab_compatible_map(comp, entity, "port", inst, tree_ports, tree_port))
      return NULL;

   return arch;
}

static tree_t elab_binding(tree_t inst, tree_t spec, elab_ctx_t *ctx)
{
   if (!tree_has_value(spec))
      return NULL;

   tree_t bind = tree_value(spec);
   assert(tree_kind(bind) == T_BINDING);

   const int ndecls = tree_decls(spec);
   if (ndecls == 0) {
      tree_t unit = tree_ref(bind);
      switch (tree_kind(unit)) {
      case T_ENTITY:
         return elab_pick_arch(tree_loc(inst), unit, ctx);
      case T_CONFIGURATION:
         return elab_root_config(unit, ctx);
      case T_ARCH:
         return unit;
      default:
         fatal_at(tree_loc(bind), "sorry, this form of binding indication is "
                  "not supported yet");
      }
   }
   else {
      assert(ndecls == 1);

      tree_t config = tree_decl(spec, 0);
      assert(tree_kind(config) == T_BLOCK_CONFIG);

      return tree_ref(config);
   }
}

static void elab_write_generic(text_buf_t *tb, tree_t value)
{
   switch (tree_kind(value)) {
   case T_LITERAL:
      switch (tree_subkind(value)) {
      case L_INT:  tb_printf(tb, "%"PRIi64, tree_ival(value)); break;
      case L_REAL: tb_printf(tb, "%lf", tree_dval(value)); break;
      }
      break;
   case T_STRING:
      {
         tb_printf(tb, "\"");
         const int nchars = tree_chars(value);
         for (int i = 0; i < nchars; i++)
            tb_append(tb, ident_char(tree_ident(tree_char(value, i)), 1));
         tb_printf(tb, "\"");
      }
      break;
   case T_AGGREGATE:
      {
         tb_append(tb, '(');
         const int nassocs = tree_assocs(value);
         for (int i = 0; i < nassocs; i++) {
            if (i > 0) tb_cat(tb, ", ");
            elab_write_generic(tb, tree_value(tree_assoc(value, i)));
         }
         tb_append(tb, ')');
      }
      break;
   case T_REF:
      tb_printf(tb, "%s", istr(tree_ident(value)));
      break;
   default:
      tb_printf(tb, "...");
   }
}

static void elab_hint_fn(diag_t *d, void *arg)
{
   tree_t t = arg;

   diag_hint(d, tree_loc(t), "while elaborating instance %s",
             istr(tree_ident(t)));

   const int ngenerics = tree_genmaps(t);
   for (int i = 0; i < ngenerics; i++) {
      tree_t p = tree_genmap(t, i);
      ident_t name = NULL;
      switch (tree_subkind(p)) {
      case P_POS:
         name = tree_ident(tree_generic(tree_ref(t), tree_pos(p)));
         break;
      case P_NAMED:
         name = tree_ident(tree_name(p));
         break;
      default:
         continue;
      }

      LOCAL_TEXT_BUF tb = tb_new();
      elab_write_generic(tb, tree_value(p));
      diag_hint(d, NULL, "generic %s => %s", istr(name), tb_get(tb));
   }
}

static tree_t elab_unconstrained_port(tree_t port, tree_t map, elab_ctx_t *ctx)
{
   type_t type = tree_type(tree_value(map));

   tree_t name = NULL;
   if (tree_subkind(map) == P_NAMED) {
      switch (tree_kind((name = tree_name(map)))) {
      case T_REF:
         break;
      case T_ARRAY_REF:
         {
            // The name is of the form X(I) so use this to derive
            // the bounds of a single-element array
            type = type_new(T_SUBTYPE);
            type_set_base(type, tree_type(port));

            tree_t left = tree_value(tree_param(name, 0));

            tree_t r = tree_new(T_RANGE);
            tree_set_subkind(r, RANGE_TO);
            tree_set_left(r, left);
            tree_set_right(r, left);

            tree_t cons = tree_new(T_CONSTRAINT);
            tree_set_subkind(cons, C_INDEX);
            tree_add_range(cons, r);

            type_add_constraint(type, cons);
         }
         break;
      default:
         error_at(tree_loc(name), "invalid formal name for unconstrained "
                  "port %s", istr(tree_ident(port)));
      }
   }

   tree_t p2 = tree_new(T_PORT_DECL);
   tree_set_ident(p2, tree_ident(port));
   tree_set_loc(p2, tree_loc(port));
   tree_set_subkind(p2, tree_subkind(port));
   tree_set_type(p2, type);
   tree_set_class(p2, tree_class(port));

   if (tree_has_value(port))
      tree_set_value(p2, tree_value(port));

   if (name != NULL) {
      tree_t ref = name_to_ref(name);
      assert(ref != NULL);
      assert(tree_ref(ref) == port);

      tree_set_ref(ref, p2);
      tree_set_type(ref, type);
   }

   // Abusing the generic rewriting mechanism to replace all
   // references to the unconstrained port
   if (ctx->generics == NULL)
      ctx->generics = hash_new(64);
   hash_put(ctx->generics, port, p2);

   return p2;
}

static void elab_ports(tree_t entity, tree_t comp, tree_t inst, elab_ctx_t *ctx)
{
   const int nports = tree_ports(entity);
   const int nparams = tree_params(inst);
   bool have_named = false;

   int binding_nparams = 0;
   tree_t binding = NULL;
   if (tree_kind(inst) == T_INSTANCE && tree_has_spec(inst)) {
      binding = tree_value(tree_spec(inst));
      binding_nparams = tree_params(binding);
   }

   for (int i = 0; i < nports; i++) {
      tree_t p = tree_port(entity, i), bp = p, map = NULL;
      ident_t pname = tree_ident(p);

      if (i < nparams && !have_named && entity == comp) {
         tree_t m = tree_param(inst, i);
         if (tree_subkind(m) == P_POS) {
            tree_t m2 = tree_new(T_PARAM);
            tree_set_loc(m2, tree_loc(m));
            tree_set_subkind(m2, P_POS);
            tree_set_pos(m2, i);
            tree_set_value(m2, tree_value(m));

            tree_add_param(ctx->out, m2);
            map = m2;
         }
      }
      else if (binding != NULL && binding_nparams) {
         // Binding may add another level of port map
         tree_t remap = NULL;
         if (i < binding_nparams) {
            tree_t m = tree_param(binding, i);
            if (tree_subkind(m) == P_POS)
               remap = tree_value(m);
         }

         if (remap == NULL) {
            for (int j = 0; j < binding_nparams; j++) {
               tree_t m = tree_param(binding, j);
               if (tree_subkind(m) == P_NAMED) {
                  tree_t name = tree_name(m);
                  tree_t ref = name_to_ref(name);
                  assert(ref != NULL);

                  if (tree_ident(ref) == pname) {
                     remap = tree_value(m);
                     break;
                  }
               }
            }
         }

         if (remap != NULL) {
            assert(tree_kind(remap) == T_REF);

            bp = tree_ref(remap);
            assert(tree_kind(bp) == T_PORT_DECL);

            pname = tree_ident(bp);
         }
      }

      if (map == NULL) {
         for (int j = 0; j < nparams; j++) {
            tree_t m = tree_param(inst, j);
            if (tree_subkind(m) == P_NAMED) {
               tree_t name = tree_name(m);
               bool is_conv = false;

               switch (tree_kind(name)) {
               case T_TYPE_CONV:
               case T_CONV_FUNC:
                  is_conv = true;
                  name = tree_value(name);
                  break;
               default:
                  break;
               }

               tree_t ref = name_to_ref(name);
               assert(ref != NULL);

               if (tree_ident(ref) != pname)
                  continue;

               if (!have_named && !is_conv && ref == name) {
                  map = tree_new(T_PARAM);
                  tree_set_loc(map, tree_loc(m));
                  tree_set_subkind(map, P_POS);
                  tree_set_pos(map, i);
                  tree_set_value(map, tree_value(m));

                  tree_add_param(ctx->out, map);
                  break;
               }

               // Make sure the map points to the right copy of the port
               // object. This is safe because elab_should_copy() always
               // copies entity ports.
               tree_set_ref(ref, p);

               tree_add_param(ctx->out, (map = m));
               have_named = true;
            }
            else if (tree_ident(tree_port(comp, tree_pos(m))) == pname) {
               map = tree_new(T_PARAM);
               tree_set_loc(map, tree_loc(m));
               tree_set_value(map, tree_value(m));

               if (!have_named) {
                  tree_set_subkind(map, P_POS);
                  tree_set_pos(map, i);
               }
               else {
                  tree_set_subkind(map, P_NAMED);
                  tree_set_name(map, make_ref(p));
                  have_named = true;
               }

               tree_add_param(ctx->out, map);
               break;
            }
         }
      }

      if (map == NULL) {
         map = tree_new(T_PARAM);
         tree_set_loc(map, tree_loc(p));

         if (have_named) {
            tree_set_subkind(map, P_NAMED);
            tree_set_name(map, make_ref(p));
         }
         else {
            tree_set_subkind(map, P_POS);
            tree_set_pos(map, i);
         }

         if (tree_has_value(p))
            tree_set_value(map, tree_value(p));
         else {
            tree_t open = tree_new(T_OPEN);
            tree_set_type(open, tree_type(p));
            tree_set_loc(open, tree_loc(p));

            tree_set_value(map, open);
         }

         tree_add_param(ctx->out, map);
      }

      if (type_is_unconstrained(tree_type(p)))
         tree_add_port(ctx->out, elab_unconstrained_port(p, map, ctx));
      else
         tree_add_port(ctx->out, p);
   }
}

static void elab_generics(tree_t entity, tree_t comp, tree_t inst,
                          elab_ctx_t *ctx)
{
   const int ngenerics = tree_generics(entity);
   const int ngenmaps = tree_genmaps(inst);

   int binding_ngenmaps = 0;
   tree_t binding = NULL;
   if (tree_kind(inst) == T_INSTANCE && tree_has_spec(inst)) {
      binding = tree_value(tree_spec(inst));
      binding_ngenmaps = tree_genmaps(binding);
   }

   if (ctx->generics == NULL && ngenerics > 0)
      ctx->generics = hash_new(ngenerics * 2);

   for (int i = 0; i < ngenerics; i++) {
      tree_t eg = tree_generic(entity, i), cg = eg;
      unsigned pos = i;
      tree_t map = NULL, bind_expr = NULL;

      if (entity != comp) {
         // Component generics may be in different order to entity
         pos = UINT_MAX;

         const int ngenerics_comp = tree_generics(comp);
         for (int j = 0; j < ngenerics_comp; j++) {
            tree_t g = tree_generic(comp, j);
            if (tree_ident(g) == tree_ident(eg)) {
               cg = g;
               pos = j;
               break;
            }
         }

         if (binding_ngenmaps > 0) {
            for (int j = 0; j < binding_ngenmaps; j++) {
               tree_t m = tree_genmap(binding, j);
               assert(tree_subkind(m) == P_POS);
               if (tree_pos(m) != i)
                  continue;

               tree_t value = tree_value(m);
               if (tree_kind(value) == T_OPEN)
                  break;
               else if (tree_kind(value) == T_REF) {
                  tree_t decl = tree_ref(value);
                  if (tree_kind(decl) == T_GENERIC_DECL) {
                     cg = tree_ref(value);
                     pos = i;
                     break;
                  }
               }

               bind_expr = value;
               break;
            }
         }
      }

      tree_add_generic(ctx->out, eg);

      if (pos < ngenmaps) {
         map = tree_genmap(inst, pos);
         assert(tree_subkind(map) == P_POS);
      }
      else if (tree_has_value(cg)) {
         map = tree_new(T_PARAM);
         tree_set_loc(map, tree_loc(cg));
         tree_set_subkind(map, P_POS);
         tree_set_pos(map, i);
         tree_set_value(map, tree_value(cg));
      }

      if (bind_expr != NULL) {
         tree_t m = tree_new(T_PARAM);
         tree_set_loc(m, tree_loc(cg));
         tree_set_subkind(m, P_POS);
         tree_set_pos(m, i);
         tree_set_value(m, bind_expr);

         // The binding expression may contain references to component
         // generics that need to be folded
         if (map != NULL) {
            hash_put(ctx->generics, cg, tree_value(map));
            simplify_global(m, ctx->generics, ctx->eval);
         }

         map = m;
      }

      if (map == NULL) {
         error_at(tree_loc(inst), "missing value for generic %s with no "
                  "default", istr(tree_ident(cg)));
         continue;
      }

      tree_add_genmap(ctx->out, map);

      tree_t value = tree_value(map);
      hash_put(ctx->generics, eg, value);
      if (eg != cg) hash_put(ctx->generics, cg, value);

      if (tree_class(eg) == C_TYPE)
         hash_put(ctx->generics, tree_type(eg), tree_type(value));
   }
}

static void elab_context(tree_t t)
{
   const int nctx = tree_contexts(t);
   for (int i = 0; i < nctx; i++) {
      // Make sure any referenced libraries are loaded to allow synth
      // binding to search for entities in them
      tree_t c = tree_context(t, i);
      if (tree_kind(c) == T_LIBRARY)
         lib_require(tree_ident(c));
   }
}

static void elab_instance(tree_t t, elab_ctx_t *ctx)
{
   tree_t arch = NULL, config = NULL;

   tree_t ref = tree_ref(t);
   switch (tree_kind(ref)) {
   case T_ENTITY:
      arch = elab_pick_arch(tree_loc(t), ref, ctx);
      break;

   case T_ARCH:
      arch = ref;
      break;

   case T_COMPONENT:
      if (tree_has_spec(t))
         arch = elab_binding(t, tree_spec(t), ctx);
      else
         arch = elab_default_binding(t, ctx);
      break;

   case T_CONFIGURATION:
      {
         config = ref;
         arch = tree_ref(tree_decl(ref, 0));
         assert(tree_kind(arch) == T_ARCH);
      }
      break;

   default:
      fatal_trace("unexpected tree kind %s in elab_instance",
                  tree_kind_str(tree_kind(ref)));
   }

   if (arch == NULL)
      return;

   tree_t b = tree_new(T_BLOCK);
   tree_set_ident(b, tree_ident(t));
   tree_set_loc(b, tree_loc(t));

   tree_add_stmt(ctx->out, b);

   ident_t ninst = hpathf(ctx->inst, '@', "%s(%s)",
                          simple_name(istr(tree_ident2(arch))),
                          simple_name(istr(tree_ident(arch))));

   lib_t new_lib = lib_require(ident_until(tree_ident(arch), '.'));

   elab_ctx_t new_ctx = {
      .out         = b,
      .root        = ctx->root,
      .path        = ctx->path,
      .inst        = ninst,
      .dotted      = ctx->dotted,
      .library     = new_lib,
      .subprograms = ctx->subprograms,
      .eval        = ctx->eval,
   };
   elab_subprogram_prefix(arch, &new_ctx);

   tree_t arch_copy;
   if (config != NULL)
      arch_copy = elab_root_config(config, &new_ctx);
   else
      arch_copy = elab_copy(arch, &new_ctx);

   tree_t entity = tree_primary(arch_copy);
   tree_t comp = primary_unit_of(tree_ref(t));

   elab_push_scope(arch, &new_ctx);
   elab_context(entity);
   elab_context(arch_copy);
   elab_generics(entity, comp, t, &new_ctx);
   simplify_global(entity, new_ctx.generics, ctx->eval);
   elab_ports(entity, comp, t, &new_ctx);
   elab_decls(entity, &new_ctx);

   if (error_count() == 0) {
      bounds_check(b);
      diag_set_hint_fn(elab_hint_fn, t);
      simplify_global(arch_copy, new_ctx.generics, ctx->eval);
      bounds_check(arch_copy);
      diag_set_hint_fn(NULL, NULL);
   }

   if (error_count() == 0)
      elab_arch(arch_copy, &new_ctx);

   elab_pop_scope(&new_ctx);
}

static void elab_decls(tree_t t, const elab_ctx_t *ctx)
{
   const int ndecls = tree_decls(t);
   for (int i = 0; i < ndecls; i++) {
      tree_t d = tree_decl(t, i);

      switch (tree_kind(d)) {
      case T_SIGNAL_DECL:
      case T_IMPLICIT_SIGNAL:
      case T_ALIAS:
      case T_FILE_DECL:
      case T_VAR_DECL:
      case T_CONST_DECL:
      case T_FUNC_BODY:
      case T_PROC_BODY:
      case T_FUNC_INST:
      case T_PROC_INST:
      case T_PROT_BODY:
      case T_TYPE_DECL:
      case T_PACK_BODY:
      case T_PACKAGE:
      case T_PACK_INST:
         tree_add_decl(ctx->out, d);
         break;
      case T_FUNC_DECL:
      case T_PROC_DECL:
         if (!is_open_coded_builtin(tree_subkind(d)))
            tree_add_decl(ctx->out, d);
         break;
      default:
         break;
      }
   }
}

static void elab_external_name(tree_t t, const elab_ctx_t *ctx)
{
   tree_t where = ctx->root, next = NULL;
   const int nparts = tree_parts(t);
   for (int i = 0; i < nparts; i++, where = next, next = NULL) {
      tree_t pe = tree_part(t, i);

      switch (tree_subkind(pe)) {
      case PE_RELATIVE:
         assert(i == 0);
         next = ctx->out;
         tree_set_subkind(pe, PE_SIMPLE);
         tree_set_ident(pe, ctx->dotted);
         continue;
      case PE_ABSOLUTE:
         assert(i == 0);
         next = ctx->root;
         tree_set_subkind(pe, PE_SIMPLE);
         tree_set_ident(pe, ident_until(tree_ident(ctx->root), '.'));
         continue;
      case PE_SIMPLE:
         break;
      case PE_GENERATE:
         {
            LOCAL_TEXT_BUF tb = tb_new();
            tb_istr(tb, tree_ident(pe));
            tb_printf(tb, "(%"PRIi64")", assume_int(tree_value(pe)));

            tree_set_subkind(pe, PE_SIMPLE);
            tree_set_ident(pe, ident_new(tb_get(tb)));
         }
         break;
      default:
         error_at(tree_loc(pe), "sorry, this form of external name is not "
                  "yet supported");
         return;
      }

      ident_t id = tree_ident(pe);

      if (is_container(where)) {
         next = search_decls(where, id, 0);

         if (next == NULL) {
            const int nstmts = tree_stmts(where);
            for (int i = 0; i < nstmts; i++) {
               tree_t s = tree_stmt(where, i);
               if (tree_ident(s) == id) {
                  next = s;
                  break;
               }
            }
         }
      }

      if (next == NULL) {
         diag_t *d = diag_new(DIAG_ERROR, tree_loc(t));
         diag_printf(d, "external name %s not found",
                     istr(tree_ident(tree_part(t, nparts - 1))));
         diag_hint(d, tree_loc(t), "name %s not found inside %s", istr(id),
                   istr(tree_ident(where)));
         diag_emit(d);
         return;
      }
   }

   assert(where != NULL);

   if (class_of(where) != tree_class(t)) {
      diag_t *d = diag_new(DIAG_ERROR, tree_loc(t));
      diag_printf(d, "class of object %s is not %s", istr(tree_ident(where)),
                  class_str(tree_class(t)));
      diag_hint(d, tree_loc(t), "external name with class %s",
                class_str(tree_class(t)));
      diag_hint(d, tree_loc(where), "declaration of %s as %s",
                istr(tree_ident(where)), class_str(class_of(where)));
      diag_emit(d);
      return;
   }
   else if (!type_eq(tree_type(where), tree_type(t))) {
      diag_t *d = diag_new(DIAG_ERROR, tree_loc(t));
      diag_printf(d, "type of %s %s is not %s",
                  class_str(tree_class(t)), istr(tree_ident(where)),
                  type_pp(tree_type(t)));
      diag_hint(d, tree_loc(t), "external name with type %s",
                type_pp(tree_type(t)));
      diag_hint(d, tree_loc(where), "declaration of %s with type %s",
                istr(tree_ident(where)), type_pp(tree_type(where)));
      diag_emit(d);
   }

   tree_set_ref(t, where);
   tree_set_type(t, tree_type(where));
}

static void elab_push_scope(tree_t t, elab_ctx_t *ctx)
{
   tree_t h = tree_new(T_HIER);
   tree_set_loc(h, tree_loc(t));
   tree_set_subkind(h, tree_kind(t));
   tree_set_ref(h, t);

   tree_set_ident(h, ctx->path);
   tree_set_ident2(h, ctx->inst);

   tree_add_decl(ctx->out, h);
}

static void elab_pop_scope(elab_ctx_t *ctx)
{
   if (ctx->generics != NULL)
      hash_free(ctx->generics);

   if (ctx->enames.count > 0) {
      // Sort the names so errors appear in a deterministic order
      qsort(ctx->enames.items, ctx->enames.count, sizeof(tree_t),
            tree_stable_compar);

      for (int i = 0; i < ctx->enames.count; i++)
         elab_external_name(ctx->enames.items[i], ctx);

      ACLEAR(ctx->enames);
   }
}

static bool elab_copy_genvar_cb(tree_t t, void *ctx)
{
   tree_t genvar = ctx;
   return tree_kind(t) == T_REF && tree_ref(t) == genvar;
}

static void elab_generate_range(tree_t r, int64_t *low, int64_t *high,
                                elab_ctx_t *ctx)
{
   if (tree_subkind(r) == RANGE_EXPR) {
      tree_t value = tree_value(r);
      assert(tree_kind(value) == T_ATTR_REF);

      tree_t tmp = tree_new(T_ATTR_REF);
      tree_set_name(tmp, tree_name(value));
      tree_set_type(tmp, tree_type(r));
      tree_set_subkind(tmp, ATTR_LOW);

      tree_t tlow = eval_must_fold(ctx->eval, tmp);
      if (folded_int(tlow, low)) {
         tree_set_subkind(tmp, ATTR_HIGH);

         tree_t thigh = eval_must_fold(ctx->eval, tmp);
         if (folded_int(thigh, high))
            return;
      }

      error_at(tree_loc(r), "generate range is not static");
      *low = *high = 0;
   }
   else
      range_bounds(r, low, high);
}

static void elab_for_generate(tree_t t, elab_ctx_t *ctx)
{
   int64_t low, high;
   elab_generate_range(tree_range(t, 0), &low, &high, ctx);

   tree_t g = tree_decl(t, 0);
   assert(tree_kind(g) == T_GENERIC_DECL);

   for (int64_t i = low; i <= high; i++) {
      LOCAL_TEXT_BUF tb = tb_new();
      tb_cat(tb, istr(tree_ident(t)));
      tb_printf(tb, "(%"PRIi64")", i);

      tree_t b = tree_new(T_BLOCK);
      tree_set_loc(b, tree_loc(t));
      tree_set_ident(b, ident_new(tb_get(tb)));
      tree_set_loc(b, tree_loc(t));

      tree_add_stmt(ctx->out, b);

      tree_t map = tree_new(T_PARAM);
      tree_set_subkind(map, P_POS);
      tree_set_loc(map, tree_loc(g));
      tree_set_value(map, get_int_lit(g, NULL, i));

      tree_add_generic(b, g);
      tree_add_genmap(b, map);

      tree_t roots[] = { t };
      tree_copy(roots, 1, elab_copy_genvar_cb, NULL, NULL, NULL, g);

      tree_t copy = roots[0];

      ident_t npath = hpathf(ctx->path, '\0', "(%"PRIi64")", i);
      ident_t ninst = hpathf(ctx->inst, '\0', "(%"PRIi64")", i);
      ident_t ndotted = hpathf(ctx->dotted, '\0', "(%"PRIi64")", i);

      elab_ctx_t new_ctx = {
         .out         = b,
         .root        = ctx->root,
         .path        = npath,
         .inst        = ninst,
         .dotted      = ndotted,
         .library     = ctx->library,
         .generics    = hash_new(16),
         .subprograms = ctx->subprograms,
         .eval        = ctx->eval,
      };

      elab_push_scope(t, &new_ctx);
      hash_put(new_ctx.generics, g, tree_value(map));

      simplify_global(copy, new_ctx.generics, new_ctx.eval);
      bounds_check(copy);

      if (error_count() == 0) {
         elab_decls(copy, &new_ctx);
         elab_stmts(copy, &new_ctx);
      }

      elab_pop_scope(&new_ctx);
   }
}

static bool elab_generate_test(tree_t value, elab_ctx_t *ctx)
{
   bool test;
   if (folded_bool(value, &test))
      return test;

   tree_t folded = eval_must_fold(ctx->eval, value);

   if (folded_bool(folded, &test))
      return test;

   error_at(tree_loc(value), "generate expression is not static");
   return false;
}

static void elab_if_generate(tree_t t, elab_ctx_t *ctx)
{
   if (elab_generate_test(tree_value(t), ctx)) {
      tree_t b = tree_new(T_BLOCK);
      tree_set_loc(b, tree_loc(t));
      tree_set_ident(b, tree_ident(t));

      tree_add_stmt(ctx->out, b);

      elab_ctx_t new_ctx = {
         .out         = b,
         .root        = ctx->root,
         .path        = ctx->path,
         .inst        = ctx->inst,
         .dotted      = ctx->dotted,
         .library     = ctx->library,
         .subprograms = ctx->subprograms,
         .eval        = ctx->eval,
      };

      elab_push_scope(t, &new_ctx);
      elab_decls(t, &new_ctx);
      elab_stmts(t, &new_ctx);
      elab_pop_scope(&new_ctx);
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
      ident_t ndotted = ident_prefix(ctx->dotted, tree_ident(s), '.');

      elab_ctx_t new_ctx = {
         .out         = ctx->out,
         .root        = ctx->root,
         .path        = npath,
         .inst        = ninst,
         .library     = ctx->library,
         .dotted      = ndotted,
         .subprograms = ctx->subprograms,
         .eval        = ctx->eval,
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
         elab_if_generate(s, &new_ctx);
         break;
      default:
         tree_add_stmt(ctx->out, s);
         break;
      }
   }
}

static void elab_block(tree_t t, const elab_ctx_t *ctx)
{
   tree_t b = tree_new(T_BLOCK);
   tree_set_ident(b, tree_ident(t));
   tree_set_loc(b, tree_loc(t));

   tree_add_stmt(ctx->out, b);

   elab_ctx_t new_ctx = {
      .out         = b,
      .root        = ctx->root,
      .path        = ctx->path,
      .inst        = ctx->inst,
      .library     = ctx->library,
      .dotted      = ctx->dotted,
      .subprograms = ctx->subprograms,
      .eval        = ctx->eval,
   };

   elab_push_scope(t, &new_ctx);
   elab_generics(t, t, t, &new_ctx);
   elab_ports(t, t, t, &new_ctx);
   elab_decls(t, &new_ctx);
   elab_stmts(t, &new_ctx);
   elab_pop_scope(&new_ctx);
}

static void elab_arch(tree_t t, const elab_ctx_t *ctx)
{
   tree_t entity = tree_primary(t);
   elab_stmts(entity, ctx);
   elab_decls(t, ctx);
   elab_stmts(t, ctx);
}

static void elab_top_level_ports(tree_t entity, const elab_ctx_t *ctx)
{
   const int nports = tree_ports(entity);
   for (int i = 0; i < nports; i++) {
      tree_t p = tree_port(entity, i);
      tree_add_port(ctx->out, p);

      tree_t m = tree_new(T_PARAM);
      tree_set_subkind(m, P_POS);
      tree_set_pos(m, i);

      if (tree_has_value(p))
         tree_set_value(m, tree_value(p));
      else {
         type_t type = tree_type(p);
         if (type_is_unconstrained(type))
            error_at(tree_loc(p), "unconnected top-level port %s cannot have "
                     "unconstrained type %s", istr(tree_ident(p)),
                     type_pp(type));

         tree_t open = tree_new(T_OPEN);
         tree_set_type(open, type);
         tree_set_loc(open, tree_loc(p));

         tree_set_value(m, open);
      }

      tree_add_param(ctx->out, m);
   }
}

static tree_t elab_generic_parse(tree_t generic, const char *str)
{
   type_t type = tree_type(generic);

   if (type_is_array(type) && type_is_enum(type_elem(type)))
      return str_to_literal(str, NULL, type);

   int64_t value;
   if (!parse_value(type, str, &value))
      fatal("failed to parse \"%s\" as type %s for generic %s",
            str, type_pp(type), istr(tree_ident(generic)));

   if (type_is_enum(type)) {
      tree_t result = tree_new(T_REF);
      tree_set_type(result, type);
      tree_set_ident(result, ident_new(str));
      tree_set_ref(result, type_enum_literal(type, value));

      return result;
   }
   else if (type_is_integer(type)) {
      tree_t result = tree_new(T_LITERAL);
      tree_set_subkind(result, L_INT);
      tree_set_type(result, type);
      tree_set_ival(result, value);

      return result;
   }
   else
      fatal("cannot override generic %s of type %s", istr(tree_ident(generic)),
            type_pp(type));
}

static void elab_top_level_generics(tree_t arch, elab_ctx_t *ctx)
{
   tree_t ent = tree_primary(arch);
   const int ngenerics = tree_generics(ent);

   if (ctx->generics == NULL && ngenerics > 0)
      ctx->generics = hash_new(ngenerics * 2);

   for (int i = 0; i < ngenerics; i++) {
      tree_t g = tree_generic(ent, i);
      ident_t name = tree_ident(g);

      generic_list_t *it;
      for (it = generic_override;
           it != NULL && it->name != name;
           it = it->next)
         ;

      tree_t value = NULL;
      if (it != NULL) {
         value = elab_generic_parse(g, it->value);
         tree_set_loc(value, tree_loc(g));
         it->used = true;
      }
      else if (tree_has_value(g))
         value = tree_value(g);
      else {
         error_at(tree_loc(g), "generic %s of top-level entity must have "
                  "default value or be specified using -gNAME=VALUE",
                  istr(name));
         continue;
      }

      tree_t map = tree_new(T_PARAM);
      tree_set_subkind(map, P_POS);
      tree_set_pos(map, i);
      tree_set_value(map, value);

      tree_add_generic(ctx->out, g);
      tree_add_genmap(ctx->out, map);

      hash_put(ctx->generics, g, value);
   }
}

static void elab_top_level(tree_t arch, const elab_ctx_t *ctx)
{
   ident_t ename = tree_ident2(arch);

   const char *name = simple_name(istr(ename));
   ident_t ninst = hpathf(ctx->inst, ':', ":%s(%s)", name,
                          simple_name(istr(tree_ident(arch))));
   ident_t npath = hpathf(ctx->path, ':', ":%s", name);
   ident_t ndotted = ident_prefix(lib_name(ctx->library), ename, '.');

   tree_t b = tree_new(T_BLOCK);
   tree_set_ident(b, ename);
   tree_set_loc(b, tree_loc(arch));

   tree_add_stmt(ctx->out, b);

   elab_ctx_t new_ctx = {
      .out         = b,
      .root        = ctx->root,
      .path        = npath,
      .inst        = ninst,
      .dotted      = ndotted,
      .library     = ctx->library,
      .subprograms = ctx->subprograms,
      .eval        = ctx->eval,
   };
   elab_subprogram_prefix(arch, &new_ctx);

   tree_t arch_copy = elab_copy(arch, &new_ctx);
   tree_t entity = tree_primary(arch_copy);

   elab_push_scope(arch, &new_ctx);
   elab_context(entity);
   elab_context(arch_copy);
   elab_top_level_generics(arch_copy, &new_ctx);
   elab_top_level_ports(entity, &new_ctx);
   elab_decls(entity, &new_ctx);

   simplify_global(arch_copy, new_ctx.generics, ctx->eval);
   bounds_check(arch);

   if (error_count() == 0)
      elab_arch(arch_copy, &new_ctx);

   elab_pop_scope(&new_ctx);
}

static vcode_unit_t elab_lower_cb(ident_t func, void *__ctx)
{
   hash_t *subprograms = __ctx;

   tree_t decl = hash_get(subprograms, func);
   if (decl == NULL)
      return NULL;

   return lower_thunk(decl);
}

void elab_set_generic(const char *name, const char *value)
{
   ident_t id = ident_new(name);

   for (generic_list_t *it = generic_override; it != NULL; it = it->next) {
      if (it->name == id)
         fatal("generic %s already has value '%s'", name, it->value);
   }

   generic_list_t *new = xmalloc(sizeof(generic_list_t));
   new->name  = id;
   new->value = xstrdup(value);
   new->used  = false;
   new->next  = generic_override;

   generic_override = new;
}

tree_t elab(tree_t top)
{
   make_new_arena();

   tree_t e = tree_new(T_ELAB);
   tree_set_ident(e, ident_prefix(tree_ident(top),
                                  ident_new("elab"), '.'));
   tree_set_loc(e, tree_loc(top));

   elab_ctx_t ctx = {
      .out      = e,
      .root     = e,
      .path     = NULL,
      .inst     = NULL,
      .library  = lib_work(),
      .eval     = eval_new(EVAL_FCALL),
   };

   ctx.subprograms = hash_new(256);

   eval_set_lower_fn(ctx.eval, elab_lower_cb, ctx.subprograms);

   switch (tree_kind(top)) {
   case T_ENTITY:
      {
         tree_t arch = elab_pick_arch(tree_loc(top), top, &ctx);
         elab_top_level(arch, &ctx);
      }
      break;
   case T_ARCH:
      elab_top_level(top, &ctx);
      break;
   case T_CONFIGURATION:
      {
         tree_t arch = elab_root_config(top, &ctx);
         elab_top_level(arch, &ctx);
      }
      break;
   default:
      fatal("%s is not a suitable top-level unit", istr(tree_ident(top)));
   }

   hash_free(ctx.subprograms);
   eval_free(ctx.eval);

   if (error_count() > 0)
      return NULL;

   if (opt_get_verbose(OPT_ELAB_VERBOSE, NULL))
      dump(e);

   for (generic_list_t *it = generic_override; it != NULL; it = it->next) {
      if (!it->used)
         warnf("generic value for %s not used", istr(it->name));
   }

   if (error_count() == 0) {
      lib_put(lib_work(), e);
      return e;
   }
   else
      return NULL;
}

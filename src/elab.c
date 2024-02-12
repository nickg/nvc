//
//  Copyright (C) 2011-2024  Nick Gasson
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
#include "cov/cov-api.h"
#include "diag.h"
#include "driver.h"
#include "eval.h"
#include "hash.h"
#include "inst.h"
#include "jit/jit.h"
#include "lib.h"
#include "lower.h"
#include "mask.h"
#include "object.h"
#include "option.h"
#include "phase.h"
#include "psl/psl-phase.h"
#include "type.h"
#include "vlog/vlog-node.h"
#include "vlog/vlog-phase.h"

#include <ctype.h>
#include <assert.h>
#include <stdarg.h>
#include <stdlib.h>
#include <inttypes.h>

typedef A(tree_t) tree_list_t;

typedef struct _elab_ctx elab_ctx_t;
typedef struct _generic_list generic_list_t;

typedef struct _elab_ctx {
   const elab_ctx_t *parent;
   tree_t            out;
   tree_t            root;
   tree_t            inst;
   ident_t           path_name;      // Current 'PATH_NAME
   ident_t           inst_name;      // Current 'INSTANCE_NAME
   ident_t           dotted;
   ident_t           prefix[2];
   lib_t             library;
   hash_t           *generics;
   jit_t            *jit;
   unit_registry_t  *registry;
   lower_unit_t     *lowered;
   cover_data_t     *cover;
   void             *context;
   driver_set_t     *drivers;
   hash_t           *shapes;
} elab_ctx_t;

typedef struct {
   ident_t     search;
   ident_t     chosen;
   timestamp_t mtime;
} lib_search_params_t;

typedef struct {
   tree_t  comp;
   tree_t *result;
} synth_binding_params_t;

typedef struct _generic_list {
   generic_list_t *next;
   ident_t         name;
   char           *value;
} generic_list_t;

static void elab_block(tree_t t, const elab_ctx_t *ctx);
static void elab_stmts(tree_t t, const elab_ctx_t *ctx);
static void elab_decls(tree_t t, const elab_ctx_t *ctx);
static void elab_push_scope(tree_t t, elab_ctx_t *ctx);
static void elab_pop_scope(elab_ctx_t *ctx);
static void elab_block_config(tree_t config, const elab_ctx_t *ctx);
static tree_t elab_copy(tree_t t, const elab_ctx_t *ctx);

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
      *p = tolower_iso88591(*p);
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

   if (kind != T_ARCH || prefix != params->search)
      return;

   const timestamp_t new_mtime = lib_get_mtime(lib, name);

   if (params->chosen == NULL) {
      params->chosen = name;
      params->mtime  = new_mtime;
   }
   else if (new_mtime > params->mtime) {
      params->chosen = name;
      params->mtime  = new_mtime;
   }
   else if (new_mtime == params->mtime) {
      // Use source file line numbers to break the tie
      tree_t old_unit = lib_get(lib, params->chosen);
      tree_t new_unit = lib_get(lib, name);

      if (old_unit == NULL)
         params->chosen = name;
      else if (new_unit != NULL) {
         const loc_t *old_loc = tree_loc(old_unit);
         const loc_t *new_loc = tree_loc(new_unit);

         if (old_loc->file_ref != new_loc->file_ref)
            warnf("cannot determine which of %s and %s is most recently "
                  "modified", istr(params->chosen), istr(name));
         else if (new_loc->first_line >= old_loc->first_line)
            params->chosen = name;
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

   lib_search_params_t params = {
      .search = search_name
   };
   lib_walk_index(lib, elab_find_arch_cb, &params);

   if (params.chosen == NULL)
      fatal_at(loc, "no suitable architecture for %s", istr(search_name));

   return lib_get(lib, params.chosen);
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

static tree_t elab_copy(tree_t t, const elab_ctx_t *ctx)
{
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

   new_instance(roots.items, roots.count, ctx->dotted, ctx->prefix,
                ARRAY_LEN(ctx->prefix));

   tree_t copy = roots.items[roots.count - 1];
   ACLEAR(roots);

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
   ident_t comp = tree_ident2(spec);

   if (tree_kind(block) == T_IF_GENERATE)
      block = tree_cond(block, 0);

   const int nstmts = tree_stmts(block);
   for (int i = 0; i < nstmts; i++) {
      tree_t s = tree_stmt(block, i);
      if (tree_kind(s) != T_INSTANCE || tree_ident2(s) != comp)
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

static tree_t elab_root_config(tree_t top, const elab_ctx_t *ctx)
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

static bool elab_synth_binding_cb(lib_t lib, void *__ctx)
{
   synth_binding_params_t *params = __ctx;

   ident_t name = ident_prefix(lib_name(lib), tree_ident(params->comp), '.');
   *(params->result) = lib_get(lib, name);

   return *(params->result) == NULL;
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

   object_t *obj = lib_get_generic(lib, full_i);

   vlog_node_t module = vlog_from_object(obj);
   if (module != NULL) {
      assert(vlog_kind(module) == V_MODULE);

      tree_t wrap = tree_new(T_VERILOG);
      tree_set_loc(wrap, tree_loc(inst));
      tree_set_ident(wrap, tree_ident(inst));
      tree_set_vlog(wrap, module);

      return wrap;
   }

   tree_t entity = tree_from_object(obj);

   if (entity == NULL && synth_binding) {
      // This is not correct according to the LRM but matches the
      // behaviour of many synthesis tools
      synth_binding_params_t params = {
         .comp     = comp,
         .result   = &entity
      };
      lib_for_all(elab_synth_binding_cb, &params);
   }

   if (entity == NULL) {
      warn_at(tree_loc(inst), "cannot find entity for component %s "
              "without binding indication", istr(tree_ident(comp)));
      return NULL;
   }

   tree_t arch = elab_pick_arch(tree_loc(comp), entity, ctx);

   // Check entity is compatible with component declaration

   tree_t bind = tree_new(T_BINDING);
   tree_set_ident(bind, tree_ident(arch));
   tree_set_loc(bind, tree_loc(arch));
   tree_set_ref(bind, arch);
   tree_set_class(bind, C_ENTITY);

   const int c_ngenerics = tree_generics(comp);
   const int e_ngenerics = tree_generics(entity);

   for (int i = 0; i < e_ngenerics; i++) {
      tree_t eg = tree_generic(entity, i);

      tree_t match = NULL;
      for (int j = 0; j < c_ngenerics; j++) {
         tree_t cg = tree_generic(comp, j);
         if (tree_ident(eg) == tree_ident(cg)) {
            match = cg;
            break;
         }
      }

      tree_t value;
      if (match != NULL) {
         type_t ctype = tree_type(match);
         type_t etype = tree_type(eg);
         if (!type_eq(ctype, etype)) {
            diag_t *d = diag_new(DIAG_ERROR, tree_loc(inst));
            diag_printf(d, "generic %s in component %s has type %s which is "
                        "incompatible with type %s in entity %s",
                        istr(tree_ident(match)), istr(tree_ident(comp)),
                        type_pp2(ctype, etype), type_pp2(etype, ctype),
                        istr(tree_ident(entity)));
            diag_hint(d, tree_loc(match), "declaration of generic %s in "
                      "component", istr(tree_ident(match)));
            diag_hint(d, tree_loc(eg), "declaration of generic %s in entity",
                      istr(tree_ident(eg)));
            diag_emit(d);
            return NULL;
         }

         value = make_ref(match);
      }
      else if (tree_has_value(eg)) {
         tree_t def = tree_value(eg);
         if (is_literal(def) && tree_kind(def) != T_STRING)
            value = def;
         else {
            tree_t open = tree_new(T_OPEN);
            tree_set_loc(open, tree_loc(eg));
            tree_set_type(open, tree_type(eg));

            value = open;
         }
      }
      else {
         diag_t *d = diag_new(DIAG_ERROR, tree_loc(inst));
         diag_printf(d, "generic %s in entity %s without a default value "
                     "has no corresponding generic in component %s",
                     istr(tree_ident(eg)),  istr(tree_ident(entity)),
                     istr(tree_ident(comp)));
         diag_hint(d, tree_loc(eg), "declaration of generic %s in entity",
                   istr(tree_ident(eg)));
         diag_emit(d);
         return NULL;
      }

      tree_t map = tree_new(T_PARAM);
      tree_set_loc(map, tree_loc(inst));
      tree_set_value(map, value);
      tree_set_subkind(map, P_POS);
      tree_set_pos(map, i);

      tree_add_genmap(bind, map);
   }

   const int c_nports = tree_ports(comp);
   const int e_nports = tree_ports(entity);

   for (int i = 0; i < e_nports; i++) {
      tree_t ep = tree_port(entity, i);

      tree_t match = NULL;
      for (int j = 0; j < c_nports; j++) {
         tree_t cp = tree_port(comp, j);
         if (tree_ident(ep) == tree_ident(cp)) {
            match = cp;
            break;
         }
      }

      tree_t value;
      if (match != NULL) {
         type_t ctype = tree_type(match);
         type_t etype = tree_type(ep);
         if (!type_eq(ctype, etype)) {
            diag_t *d = diag_new(DIAG_ERROR, tree_loc(inst));
            diag_printf(d, "port %s in component %s has type %s which is "
                        "incompatible with type %s in entity %s",
                        istr(tree_ident(match)), istr(tree_ident(comp)),
                        type_pp2(ctype, etype), type_pp2(etype, ctype),
                        istr(tree_ident(entity)));
            diag_hint(d, tree_loc(match), "declaration of port %s in component",
                      istr(tree_ident(match)));
            diag_hint(d, tree_loc(ep), "declaration of port %s in entity",
                      istr(tree_ident(ep)));
            diag_emit(d);
            return NULL;
         }

         value = make_ref(match);
      }
      else {
         const bool open_ok =
            tree_has_value(ep)
            || (tree_subkind(ep) == PORT_OUT
                && !type_is_unconstrained(tree_type(ep)));

          if (open_ok) {
             tree_t open = tree_new(T_OPEN);
             tree_set_loc(open, tree_loc(ep));
             tree_set_type(open, tree_type(ep));

             value = open;
          }
          else {
            diag_t *d = diag_new(DIAG_ERROR, tree_loc(inst));
            diag_printf(d, "port %s in entity %s without a default value "
                        "has no corresponding port in component %s",
                        istr(tree_ident(ep)), istr(tree_ident(entity)),
                        istr(tree_ident(comp)));
            diag_hint(d, tree_loc(ep), "port %s declared here",
                      istr(tree_ident(ep)));
            diag_emit(d);
            return NULL;
         }
      }

      add_param(bind, value, P_POS, NULL);
   }

   return bind;
}

static tree_t elab_binding(tree_t inst, tree_t spec, const elab_ctx_t *ctx)
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
   case T_TYPE_CONV:
   case T_QUALIFIED:
      elab_write_generic(tb, tree_value(value));
      break;
   case T_TYPE_REF:
      tb_printf(tb, "%s", type_pp(tree_type(value)));
      break;
   case T_OPEN:
      tb_cat(tb, "OPEN");
      break;
   default:
      tb_printf(tb, "...");
      DEBUG_ONLY(tb_cat(tb, tree_kind_str(tree_kind(value))));
   }
}

static void elab_hint_fn(diag_t *d, void *arg)
{
   tree_t t = arg;

   diag_hint(d, tree_loc(t), "while elaborating instance %s",
             istr(tree_ident(t)));

   tree_t unit = tree_ref(t);
   const tree_kind_t kind = tree_kind(unit);
   if (kind == T_CONFIGURATION || kind == T_ARCH)
      unit = tree_primary(unit);

   const int ngenerics = tree_genmaps(t);
   for (int i = 0; i < ngenerics; i++) {
      tree_t p = tree_genmap(t, i);
      ident_t name = NULL;
      switch (tree_subkind(p)) {
      case P_POS:
         name = tree_ident(tree_generic(unit, tree_pos(p)));
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

static void elab_ports(tree_t entity, tree_t bind, const elab_ctx_t *ctx)
{
   const int nports = tree_ports(entity);
   const int nparams = tree_params(bind);
   bool have_named = false;

   for (int i = 0; i < nports; i++) {
      tree_t p = tree_port(entity, i), map = NULL;
      ident_t pname = tree_ident(p);

      if (i < nparams && !have_named) {
         tree_t m = tree_param(bind, i);
         if (tree_subkind(m) == P_POS) {
            assert(tree_pos(m) == i);
            tree_add_param(ctx->out, m);
            map = m;
         }
      }

      if (map == NULL) {
         for (int j = 0; j < nparams; j++) {
            tree_t m = tree_param(bind, j);
            if (tree_subkind(m) == P_NAMED) {
               tree_t name = tree_name(m), ref;
               bool is_conv = false;

               switch (tree_kind(name)) {
               case T_TYPE_CONV:
               case T_CONV_FUNC:
                  is_conv = true;
                  ref = name_to_ref(tree_value(name));
                  break;
               default:
                  ref = name_to_ref(name);
                  break;
               }
               assert(ref != NULL);

               if (tree_ident(ref) != pname)
                  continue;

               map = tree_new(T_PARAM);
               tree_set_loc(map, tree_loc(m));
               tree_set_value(map, tree_value(m));

               tree_add_param(ctx->out, map);

               if (!have_named && !is_conv && ref == name) {
                  tree_set_subkind(map, P_POS);
                  tree_set_pos(map, i);
                  break;
               }
               else {
                  tree_set_subkind(map, P_NAMED);
                  tree_set_name(map, change_ref(tree_name(m), p));
                  have_named = true;
               }
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

      tree_add_port(ctx->out, p);
   }
}

static tree_t elab_parse_generic_string(tree_t generic, const char *str)
{
   type_t type = tree_type(generic);

   parsed_value_t value;
   if (!parse_value(type, str, &value))
      fatal("failed to parse \"%s\" as type %s for generic %s",
            str, type_pp(type), istr(tree_ident(generic)));

   if (type_is_enum(type)) {
      type_t base = type_base_recur(type);
      tree_t lit = type_enum_literal(base, value.integer);

      tree_t result = tree_new(T_REF);
      tree_set_type(result, type);
      tree_set_ident(result, ident_new(str));
      tree_set_ref(result, lit);
      tree_set_loc(result, tree_loc(generic));

      return result;
   }
   else if (type_is_integer(type)) {
      tree_t result = tree_new(T_LITERAL);
      tree_set_subkind(result, L_INT);
      tree_set_type(result, type);
      tree_set_ival(result, value.integer);
      tree_set_loc(result, tree_loc(generic));

      return result;
   }
   else if (type_is_real(type)) {
      tree_t result = tree_new(T_LITERAL);
      tree_set_subkind(result, L_REAL);
      tree_set_type(result, type);
      tree_set_dval(result, value.real);
      tree_set_loc(result, tree_loc(generic));

      return result;
   }
   else if (type_is_physical(type)) {
      tree_t result = tree_new(T_LITERAL);
      tree_set_subkind(result, L_PHYSICAL);
      tree_set_type(result, type);
      tree_set_ival(result, value.integer);
      tree_set_loc(result, tree_loc(generic));

      return result;
   }
   else if (type_is_character_array(type)) {
      tree_t t = tree_new(T_STRING);
      tree_set_loc(t, tree_loc(generic));

      type_t elem = type_base_recur(type_elem(type));
      for (int i = 0; i < value.enums->count; i++) {
         tree_t lit = type_enum_literal(elem, value.enums->values[i]);

         tree_t ref = tree_new(T_REF);
         tree_set_ident(ref, tree_ident(lit));
         tree_set_ref(ref, lit);
         tree_add_char(t, ref);
      }
      free(value.enums);

      tree_set_type(t, subtype_for_string(t, type));
      return t;
   }
   else
      fatal("cannot override generic %s of type %s", istr(tree_ident(generic)),
            type_pp(type));
}

static tree_t elab_find_generic_override(tree_t g, const elab_ctx_t *ctx)
{
   if (generic_override == NULL)
      return NULL;

   ident_t qual = tree_ident(g);
   for (const elab_ctx_t *e = ctx; e->inst; e = e->parent)
      qual = ident_prefix(tree_ident(e->inst), qual, '.');

   generic_list_t **it, *tmp;
   for (it = &generic_override;
        *it && (*it)->name != qual;
        it = &((*it)->next))
      ;

   if (*it == NULL)
      return NULL;

   tree_t value = elab_parse_generic_string(g, (*it)->value);

   *it = (tmp = (*it))->next;
   free(tmp->value);
   free(tmp);

   return value;
}

static void elab_generics(tree_t entity, tree_t bind, elab_ctx_t *ctx)
{
   const int ngenerics = tree_generics(entity);
   const int ngenmaps = tree_genmaps(bind);

   for (int i = 0; i < ngenerics; i++) {
      tree_t g = tree_generic(entity, i);
      tree_add_generic(ctx->out, g);

      tree_t map = NULL;
      if (i < ngenmaps) {
         map = tree_genmap(bind, i);
         assert(tree_subkind(map) == P_POS);
         assert(tree_pos(map) == i);
      }
      else if (tree_has_value(g)) {
         map = tree_new(T_PARAM);
         tree_set_loc(map, tree_loc(g));
         tree_set_subkind(map, P_POS);
         tree_set_pos(map, i);
         tree_set_value(map, tree_value(g));
      }

      tree_t override = elab_find_generic_override(g, ctx);
      if (override != NULL) {
         map = tree_new(T_PARAM);
         tree_set_subkind(map, P_POS);
         tree_set_pos(map, i);
         tree_set_value(map, override);
      }

      if (map == NULL) {
         error_at(tree_loc(bind), "missing value for generic %s with no "
                  "default", istr(tree_ident(g)));
         continue;
      }

      tree_t value = tree_value(map);

      switch (tree_kind(value)) {
      case T_REF:
         if (tree_kind(tree_ref(value)) == T_ENUM_LIT)
            break;
         else if (tree_class(g) == C_PACKAGE)
            break;
         // Fall-through
      case T_ARRAY_REF:
      case T_RECORD_REF:
      case T_FCALL:
         if (type_is_scalar(tree_type(value))) {
            tree_t folded = eval_try_fold(ctx->jit, value,
                                          ctx->parent->lowered,
                                          ctx->parent->context);

            if (folded != value) {
               tree_t m = tree_new(T_PARAM);
               tree_set_loc(m, tree_loc(map));
               tree_set_subkind(m, P_POS);
               tree_set_pos(m, tree_pos(map));
               tree_set_value(m, (value = folded));

               map = m;
            }
         }
         break;

      default:
         break;
      }

      tree_add_genmap(ctx->out, map);

      if (is_literal(value)) {
         // These values can be safely substituted for all references to
         // the generic name
         if (ctx->generics == NULL)
            ctx->generics = hash_new(ngenerics * 2);
         hash_put(ctx->generics, g, value);
      }
   }
}

static void elab_instance_fixup(tree_t arch, const elab_ctx_t *ctx)
{
   if (standard() < STD_08)
      return;

   hash_t *map = NULL;

   const int ngenerics = tree_generics(ctx->out);
   assert(tree_genmaps(ctx->out) == ngenerics);

   for (int i = 0; i < ngenerics; i++) {
      tree_t g = tree_generic(ctx->out, i);

      const class_t class = tree_class(g);
      if (class == C_CONSTANT)
         continue;
      else if (map == NULL)
         map = hash_new(64);

      tree_t value = tree_value(tree_genmap(ctx->out, i));

      switch (class) {
      case C_TYPE:
         hash_put(map, tree_type(g), tree_type(value));
         break;

      case C_PACKAGE:
         {
            tree_t formal = tree_ref(tree_value(g));
            tree_t actual = tree_ref(value);

            const int ndecls = tree_decls(formal);
            for (int i = 0; i < ndecls; i++) {
               tree_t gd = tree_decl(formal, i);
               tree_t ad = tree_decl(actual, i);
               assert(tree_kind(gd) == tree_kind(ad));

               hash_put(map, gd, ad);

               if (is_type_decl(gd))
                  hash_put(map, tree_type(gd), tree_type(ad));
            }

            const int ngenerics = tree_generics(formal);
            for (int i = 0; i < ngenerics; i++)
               hash_put(map, tree_generic(formal, i), tree_generic(actual, i));
         }
         break;

      case C_FUNCTION:
      case C_PROCEDURE:
         hash_put(map, g, tree_ref(value));
         break;

      default:
         break;
      }
   }

   if (map == NULL)
      return;

   instance_fixup(arch, map);
   hash_free(map);
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

static void elab_inherit_context(elab_ctx_t *ctx, const elab_ctx_t *parent)
{
   ctx->parent    = parent;
   ctx->jit       = parent->jit;
   ctx->registry  = parent->registry;
   ctx->root      = parent->root;
   ctx->dotted    = ctx->dotted ?: parent->dotted;
   ctx->path_name = ctx->path_name ?: parent->path_name;
   ctx->inst_name = ctx->inst_name ?: parent->inst_name;
   ctx->library   = ctx->library ?: parent->library;
   ctx->out       = ctx->out ?: parent->out;
   ctx->cover     = parent->cover;
   ctx->inst      = ctx->inst ?: parent->inst;
   ctx->shapes    = parent->shapes;
}

static driver_set_t *elab_driver_set(const elab_ctx_t *ctx)
{
   if (ctx->drivers != NULL)
      return ctx->drivers;
   else if (ctx->parent != NULL)
      return elab_driver_set(ctx->parent);
   else
      return NULL;
}

static void elab_lower(tree_t b, vcode_unit_t shape, elab_ctx_t *ctx)
{
   ctx->lowered = lower_instance(ctx->registry, ctx->parent->lowered, shape,
                                 elab_driver_set(ctx), ctx->cover, b);

   if (error_count() > 0)
      return;   // Could be errors resolving external names

   if (ctx->inst != NULL)
      diag_add_hint_fn(elab_hint_fn, ctx->inst);

   ctx->context = eval_instance(ctx->jit, ctx->dotted, ctx->parent->context);

   if (ctx->inst != NULL)
      diag_remove_hint_fn(elab_hint_fn);
}

static void elab_mixed_port_map(tree_t block, vlog_node_t mod,
                                const elab_ctx_t *ctx)
{
   const int nports = tree_ports(block);
   const int ndecls = vlog_decls(mod);

   bit_mask_t have;
   mask_init(&have, nports);

   type_t std_logic = ieee_type(IEEE_STD_LOGIC);

#define T_LOGIC "19NVC.VERILOG.T_LOGIC"
   ident_t to_vhdl_name = ident_new("NVC.VERILOG.TO_VHDL(" T_LOGIC ")U");
   ident_t to_verilog_name = ident_new("NVC.VERILOG.TO_VERILOG(U)" T_LOGIC);

   tree_t to_vhdl = verilog_func(to_vhdl_name);
   tree_t to_verilog = verilog_func(to_verilog_name);

   bool have_named = false;
   for (int i = 0; i < ndecls; i++) {
      vlog_node_t mport = vlog_decl(mod, i);
      if (vlog_kind(mport) != V_PORT_DECL)
         continue;

      ident_t name = vlog_ident2(mport);

      tree_t vport = tree_port(ctx->out, i);
      assert(tree_ident(vport) == vlog_ident(mport));

      int bpos = 0;
      tree_t bport = NULL;
      for (; bpos < nports; bpos++) {
         tree_t pj = tree_port(block, bpos);
         if (tree_ident(pj) == name) {
            bport = pj;
            mask_set(&have, bpos);
            break;
         }
      }

      if (bport == NULL) {
         tree_t comp = tree_ref(ctx->inst);
         assert(tree_kind(comp) == T_COMPONENT);

         error_at(tree_loc(comp), "missing matching VHDL port declaration for "
                  "Verilog port %s in component %s", istr(vlog_ident(mport)),
                  istr(tree_ident(comp)));
         return;
      }

      if (vlog_ident2(mport) != tree_ident(bport)) {
         tree_t comp = tree_ref(ctx->inst);
         assert(tree_kind(comp) == T_COMPONENT);

         error_at(tree_loc(bport), "expected VHDL port name %s to match "
                  "Verilog port name %s in component %s",
                  istr(tree_ident(bport)), istr(vlog_ident(mport)),
                  istr(tree_ident(comp)));
         return;
      }

      type_t type = tree_type(bport);
      if (!type_eq(type, std_logic)) {
         error_at(tree_loc(bport), "Verilog module ports must have "
                  "type STD_LOGIC or STD_LOGIC_VECTOR");
         return;
      }

      if (vlog_subkind(mport) == V_PORT_INPUT) {
         tree_t conv = tree_new(T_CONV_FUNC);
         tree_set_loc(conv, tree_loc(bport));
         tree_set_ref(conv, to_verilog);
         tree_set_ident(conv, tree_ident(to_verilog));
         tree_set_type(conv, type_result(tree_type(to_verilog)));
         tree_set_value(conv, make_ref(bport));

         if (have_named)
            add_param(ctx->out, conv, P_NAMED, make_ref(vport));
         else
            add_param(ctx->out, conv, P_POS, NULL);
      }
      else {
         tree_t conv = tree_new(T_CONV_FUNC);
         tree_set_loc(conv, tree_loc(bport));
         tree_set_ref(conv, to_vhdl);
         tree_set_ident(conv, tree_ident(to_vhdl));
         tree_set_type(conv, type_result(tree_type(to_vhdl)));
         tree_set_value(conv, make_ref(vport));

         add_param(ctx->out, make_ref(bport), P_NAMED, conv);
         have_named = true;
      }

      bpos++;
   }

   for (int i = 0; i < nports; i++) {
      if (!mask_test(&have, i)) {
         tree_t p = tree_port(block, i);
         diag_t *d = diag_new(DIAG_ERROR, tree_loc(p));
         diag_printf(d, "port %s not found in Verilog module %s",
                     istr(tree_ident(p)), istr(vlog_ident2(mod)));
         diag_emit(d);
      }
   }

   mask_free(&have);
}

static void elab_verilog_module(tree_t wrap, const elab_ctx_t *ctx)
{
   vlog_node_t mod = tree_vlog(wrap);

   vcode_unit_t shape = hash_get(ctx->shapes, mod);
   if (shape == NULL) {
      shape = vlog_lower(ctx->registry, wrap);
      hash_put(ctx->shapes, mod, shape);
   }

   ident_t label = ident_rfrom(vlog_ident(mod), '.');

   const char *label_str = istr(label);
   ident_t npath = hpathf(ctx->path_name, ':', "%s", label_str);
   ident_t ninst = hpathf(ctx->inst_name, ':', "%s", label_str);
   ident_t ndotted = ident_prefix(ctx->dotted, label, '.');

   elab_ctx_t new_ctx = {
      .path_name = npath,
      .inst_name = ninst,
      .dotted    = ndotted,
   };
   elab_inherit_context(&new_ctx, ctx);

   tree_t b = tree_new(T_BLOCK);
   tree_set_ident(b, label);
   tree_set_loc(b, tree_loc(ctx->out));

   tree_add_stmt(ctx->out, b);
   new_ctx.out = b;

   elab_push_scope(wrap, &new_ctx);
   vlog_trans(mod, b);

   const int nstmts = vlog_stmts(mod);
   for (int i = 0; i < nstmts; i++) {
      vlog_node_t s = vlog_stmt(mod, i);

      tree_t w = tree_new(T_VERILOG);
      tree_set_ident(w, vlog_ident(s));
      tree_set_loc(w, vlog_loc(s));
      tree_set_vlog(w, s);

      tree_add_stmt(b, w);
   }

   if (ctx->inst != NULL)
      elab_mixed_port_map(ctx->out, mod, &new_ctx);

   if (error_count() == 0)
      elab_lower(b, shape, &new_ctx);

   elab_pop_scope(&new_ctx);
}

static void elab_architecture(tree_t bind, tree_t arch, const elab_ctx_t *ctx)
{
   tree_t inst = NULL;
   ident_t label, ninst = NULL, npath = NULL;
   switch (tree_kind(bind)) {
   case T_BINDING:
      label = ident_rfrom(tree_ident(tree_primary(arch)), '.');
      break;
   case T_INSTANCE:
      {
         label = tree_ident(bind);
         inst = bind;

         const char *label_str = istr(label);
         npath = hpathf(ctx->path_name, ':', "%s", label_str);
         ninst = hpathf(ctx->inst_name, ':', "%s", label_str);
      }
      break;
   default:
      fatal_trace("unexpected binding kind %s in elab_architecture",
                  tree_kind_str(tree_kind(bind)));
   }

   ident_t ndotted = ident_prefix(ctx->dotted, label, '.');

   elab_ctx_t new_ctx = {
      .path_name = npath,
      .inst_name = ninst,
      .dotted    = ndotted,
      .inst      = inst,
   };
   elab_inherit_context(&new_ctx, ctx);

   tree_t b = tree_new(T_BLOCK);
   tree_set_ident(b, label);
   tree_set_loc(b, tree_loc(bind));

   tree_add_stmt(ctx->out, b);
   new_ctx.out = b;

   new_ctx.inst_name = hpathf(new_ctx.inst_name, '@', "%s(%s)",
                              simple_name(istr(tree_ident2(arch))),
                              simple_name(istr(tree_ident(arch))));
   new_ctx.library = lib_require(ident_until(tree_ident(arch), '.'));

   elab_subprogram_prefix(arch, &new_ctx);

   tree_t arch_copy = elab_copy(arch, &new_ctx);
   tree_t entity = tree_primary(arch_copy);

   elab_push_scope(arch, &new_ctx);
   elab_context(entity);
   elab_context(arch_copy);
   elab_generics(entity, bind, &new_ctx);
   elab_instance_fixup(arch_copy, &new_ctx);
   simplify_global(arch_copy, new_ctx.generics, ctx->jit, ctx->registry);
   elab_ports(entity, bind, &new_ctx);
   elab_decls(entity, &new_ctx);

   if (error_count() == 0)
      elab_decls(arch_copy, &new_ctx);

   if (error_count() == 0) {
      new_ctx.drivers = find_drivers(arch_copy);
      elab_lower(b, NULL, &new_ctx);
      elab_stmts(entity, &new_ctx);
      elab_stmts(arch_copy, &new_ctx);
   }

   elab_pop_scope(&new_ctx);
}

static void elab_component(tree_t inst, tree_t comp, const elab_ctx_t *ctx)
{
   tree_t arch, bind;
   if (tree_has_spec(inst)) {
      tree_t spec = tree_spec(inst);
      arch = elab_binding(inst, spec, ctx);
      bind = arch ? tree_value(spec) : NULL;
   }
   else {
      bind = elab_default_binding(inst, ctx);
      if (bind == NULL)
         arch = NULL;
      else if (tree_kind(bind) == T_VERILOG)
         arch = bind;   // XXX: generate binding for Verilog module too
      else
         arch = tree_ref(bind);
   }

   const char *label = istr(tree_ident(inst));
   ident_t npath = hpathf(ctx->path_name, ':', "%s", label);
   ident_t ninst = hpathf(ctx->inst_name, ':', "%s", label);
   ident_t ndotted = ident_prefix(ctx->dotted, tree_ident(inst), '.');

   elab_ctx_t new_ctx = {
      .path_name = npath,
      .inst_name = ninst,
      .dotted    = ndotted,
      .inst      = inst,
   };
   elab_inherit_context(&new_ctx, ctx);

   tree_t b = tree_new(T_BLOCK);
   tree_set_ident(b, tree_ident(inst));
   tree_set_loc(b, tree_loc(inst));

   tree_add_stmt(ctx->out, b);
   new_ctx.out = b;

   elab_push_scope(comp, &new_ctx);
   elab_generics(comp, inst, &new_ctx);
   elab_ports(comp, inst, &new_ctx);

   if (bind != NULL && tree_kind(bind) != T_VERILOG)   // XXX: temporary
      new_ctx.drivers = find_drivers(bind);

   if (error_count() == 0)
      elab_lower(b, NULL, &new_ctx);

   if (arch == NULL)
      ;   // Unbound architecture
   else if (tree_kind(arch) == T_VERILOG)
      elab_verilog_module(arch, &new_ctx);
   else if (error_count() == 0)
      elab_architecture(bind, arch, &new_ctx);

   elab_pop_scope(&new_ctx);
}

static void elab_instance(tree_t t, const elab_ctx_t *ctx)
{
   tree_t ref = tree_ref(t);
   switch (tree_kind(ref)) {
   case T_ENTITY:
      {
         tree_t arch = elab_pick_arch(tree_loc(t), ref, ctx);
         elab_architecture(t, arch, ctx);
      }
      break;

   case T_ARCH:
      elab_architecture(t, ref, ctx);
      break;

   case T_COMPONENT:
      elab_component(t, ref, ctx);
      break;

   case T_CONFIGURATION:
      {
         tree_t arch = elab_root_config(ref, ctx);
         elab_architecture(t, arch, ctx);
      }
      break;

   default:
      fatal_trace("unexpected tree kind %s in elab_instance",
                  tree_kind_str(tree_kind(ref)));
   }
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
      case T_PROT_DECL:
      case T_PROT_BODY:
      case T_TYPE_DECL:
      case T_SUBTYPE_DECL:
      case T_PACK_BODY:
      case T_PACKAGE:
      case T_PACK_INST:
      case T_PSL:
      case T_ATTR_SPEC:
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

static void elab_push_scope(tree_t t, elab_ctx_t *ctx)
{
   tree_t h = tree_new(T_HIER);
   tree_set_loc(h, tree_loc(t));
   tree_set_subkind(h, tree_kind(t));
   tree_set_ref(h, t);

   tree_set_ident(h, ctx->path_name);
   tree_set_ident2(h, ctx->inst_name);

   tree_add_decl(ctx->out, h);
}

static void elab_pop_scope(elab_ctx_t *ctx)
{
   if (ctx->generics != NULL)
      hash_free(ctx->generics);

   if (ctx->drivers != NULL)
      free_drivers(ctx->drivers);

   cover_pop_scope(ctx->cover);

   if (ctx->lowered != NULL)
      unit_registry_finalise(ctx->registry, ctx->lowered);
}

static bool elab_copy_genvar_cb(tree_t t, void *ctx)
{
   tree_t genvar = ctx;
   return tree_kind(t) == T_REF && tree_ref(t) == genvar;
}

static void elab_generate_range(tree_t r, int64_t *low, int64_t *high,
                                const elab_ctx_t *ctx)
{
   if (tree_subkind(r) == RANGE_EXPR) {
      tree_t value = tree_value(r);
      assert(tree_kind(value) == T_ATTR_REF);

      tree_t tmp = tree_new(T_ATTR_REF);
      tree_set_name(tmp, tree_name(value));
      tree_set_type(tmp, tree_type(r));
      tree_set_subkind(tmp, ATTR_LOW);

      tree_t tlow = eval_must_fold(ctx->jit, tmp, ctx->lowered, ctx->context);
      if (folded_int(tlow, low)) {
         tree_set_subkind(tmp, ATTR_HIGH);

         tree_t thigh = eval_must_fold(ctx->jit, tmp, ctx->lowered,
                                       ctx->context);
         if (folded_int(thigh, high))
            return;
      }

      error_at(tree_loc(r), "generate range is not static");
      *low = *high = 0;
   }
   else if (!folded_bounds(r, low, high)) {
      tree_t left  = eval_must_fold(ctx->jit, tree_left(r),
                                    ctx->lowered, ctx->context);
      tree_t right = eval_must_fold(ctx->jit, tree_right(r),
                                    ctx->lowered, ctx->context);

      int64_t ileft, iright;
      if (folded_int(left, &ileft) && folded_int(right, &iright)) {
         const bool asc = (tree_subkind(r) == RANGE_TO);
         *low = asc ? ileft : iright;
         *high = asc ? iright : ileft;
      }
      else {
         error_at(tree_loc(r), "generate range is not static");
         *low = *high = 0;
      }
   }
}

static void elab_for_generate(tree_t t, const elab_ctx_t *ctx)
{
   int64_t low, high;
   elab_generate_range(tree_range(t, 0), &low, &high, ctx);

   tree_t g = tree_decl(t, 0);
   assert(tree_kind(g) == T_GENERIC_DECL);

   ident_t base = tree_ident(t);

   for (int64_t i = low; i <= high; i++) {
      LOCAL_TEXT_BUF tb = tb_new();
      tb_cat(tb, istr(base));
      tb_printf(tb, "(%"PRIi64")", i);

      ident_t id = ident_new(tb_get(tb));

      tree_t b = tree_new(T_BLOCK);
      tree_set_loc(b, tree_loc(t));
      tree_set_ident(b, id);
      tree_set_loc(b, tree_loc(t));

      tree_add_stmt(ctx->out, b);

      tree_t map = tree_new(T_PARAM);
      tree_set_subkind(map, P_POS);
      tree_set_loc(map, tree_loc(g));
      tree_set_value(map, get_int_lit(g, NULL, i));

      tree_add_generic(b, g);
      tree_add_genmap(b, map);

      const char *label = istr(base);
      ident_t npath = hpathf(ctx->path_name, ':', "%s(%"PRIi64")", label, i);
      ident_t ninst = hpathf(ctx->inst_name, ':', "%s(%"PRIi64")", label, i);
      ident_t ndotted = ident_prefix(ctx->dotted, id, '.');

      elab_ctx_t new_ctx = {
         .out       = b,
         .path_name = npath,
         .inst_name = ninst,
         .dotted    = ndotted,
         .generics  = hash_new(16),
      };
      elab_inherit_context(&new_ctx, ctx);

      new_ctx.prefix[0] = ident_prefix(ctx->dotted, base, '.');

      tree_t roots[] = { t };
      copy_with_renaming(roots, 1, elab_copy_genvar_cb, NULL, g, ndotted,
                         new_ctx.prefix, ARRAY_LEN(new_ctx.prefix));

      tree_t copy = roots[0];

      elab_push_scope(t, &new_ctx);
      hash_put(new_ctx.generics, g, tree_value(map));

      simplify_global(copy, new_ctx.generics, new_ctx.jit, new_ctx.registry);

      new_ctx.drivers = find_drivers(copy);

      if (error_count() == 0)
         elab_decls(copy, &new_ctx);

      if (error_count() == 0) {
         elab_lower(b, NULL, &new_ctx);
         elab_stmts(copy, &new_ctx);
      }

      elab_pop_scope(&new_ctx);
   }
}

static bool elab_generate_test(tree_t value, const elab_ctx_t *ctx)
{
   bool test;
   if (folded_bool(value, &test))
      return test;

   tree_t folded = eval_must_fold(ctx->jit, value, ctx->lowered, ctx->context);

   if (folded_bool(folded, &test))
      return test;

   error_at(tree_loc(value), "generate expression is not static");
   return false;
}

static void elab_if_generate(tree_t t, const elab_ctx_t *ctx)
{
   const int nconds = tree_conds(t);
   for (int i = 0; i < nconds; i++) {
      tree_t cond = tree_cond(t, i);
      if (!tree_has_value(cond) || elab_generate_test(tree_value(cond), ctx)) {
         tree_t b = tree_new(T_BLOCK);
         tree_set_loc(b, tree_loc(cond));
         tree_set_ident(b, tree_ident(cond));

         tree_add_stmt(ctx->out, b);

         const char *label = istr(tree_ident(cond));
         ident_t npath = hpathf(ctx->path_name, ':', "%s", label);
         ident_t ninst = hpathf(ctx->inst_name, ':', "%s", label);
         ident_t ndotted = ident_prefix(ctx->dotted, tree_ident(cond), '.');

         elab_ctx_t new_ctx = {
            .out       = b,
            .path_name = npath,
            .inst_name = ninst,
            .dotted    = ndotted,
         };
         elab_inherit_context(&new_ctx, ctx);

         elab_push_scope(t, &new_ctx);
         elab_decls(cond, &new_ctx);

         new_ctx.drivers = find_drivers(cond);

         if (error_count() == 0) {
            elab_lower(b, NULL, &new_ctx);
            elab_stmts(cond, &new_ctx);
         }

         elab_pop_scope(&new_ctx);
         return;
      }
   }
}

static void elab_case_generate(tree_t t, const elab_ctx_t *ctx)
{
   tree_t chosen = eval_case(ctx->jit, t, ctx->lowered, ctx->context);
   if (chosen == NULL)
      return;

   ident_t id = tree_has_ident(chosen) ? tree_ident(chosen) : tree_ident(t);

   tree_t b = tree_new(T_BLOCK);
   tree_set_loc(b, tree_loc(chosen));
   tree_set_ident(b, id);

   tree_add_stmt(ctx->out, b);

   const char *label = istr(id);
   ident_t npath = hpathf(ctx->path_name, ':', "%s", label);
   ident_t ninst = hpathf(ctx->inst_name, ':', "%s", label);
   ident_t ndotted = ident_prefix(ctx->dotted, id, '.');

   elab_ctx_t new_ctx = {
      .out       = b,
      .path_name = npath,
      .inst_name = ninst,
      .dotted    = ndotted,
   };
   elab_inherit_context(&new_ctx, ctx);

   elab_push_scope(t, &new_ctx);
   elab_decls(chosen, &new_ctx);

   new_ctx.drivers = find_drivers(chosen);

   if (error_count() == 0) {
      elab_lower(b, NULL, &new_ctx);
      elab_stmts(chosen, &new_ctx);
   }

   elab_pop_scope(&new_ctx);
}

static void elab_process(tree_t t, const elab_ctx_t *ctx)
{
   if (error_count() == 0)
      lower_process(ctx->lowered, t, elab_driver_set(ctx));

   tree_add_stmt(ctx->out, t);
}

static void elab_psl(tree_t t, const elab_ctx_t *ctx)
{
   if (error_count() == 0)
      psl_lower_directive(ctx->registry, ctx->lowered, ctx->cover, t);

   tree_add_stmt(ctx->out, t);
}

static void elab_stmts(tree_t t, const elab_ctx_t *ctx)
{
   const int nstmts = tree_stmts(t);
   for (int i = 0; i < nstmts; i++) {
      tree_t s = tree_stmt(t, i);

      switch (tree_kind(s)) {
      case T_INSTANCE:
         elab_instance(s, ctx);
         break;
      case T_BLOCK:
         elab_block(s, ctx);
         break;
      case T_FOR_GENERATE:
         elab_for_generate(s, ctx);
         break;
      case T_IF_GENERATE:
         elab_if_generate(s, ctx);
         break;
      case T_CASE_GENERATE:
         elab_case_generate(s, ctx);
         break;
      case T_PROCESS:
         elab_process(s, ctx);
         break;
      case T_PSL:
         elab_psl(s, ctx);
         break;
      default:
         fatal_trace("unexpected statement %s", tree_kind_str(tree_kind(s)));
      }
   }
}

static void elab_block(tree_t t, const elab_ctx_t *ctx)
{
   ident_t id = tree_ident(t);

   tree_t b = tree_new(T_BLOCK);
   tree_set_ident(b, id);
   tree_set_loc(b, tree_loc(t));

   tree_add_stmt(ctx->out, b);

   const char *label = istr(id);
   ident_t npath = hpathf(ctx->path_name, ':', "%s", label);
   ident_t ninst = hpathf(ctx->inst_name, ':', "%s", label);
   ident_t ndotted = ident_prefix(ctx->dotted, id, '.');

   elab_ctx_t new_ctx = {
      .out       = b,
      .path_name = npath,
      .inst_name = ninst,
      .dotted    = ndotted,
   };
   elab_inherit_context(&new_ctx, ctx);

   const int base_errors = error_count();

   elab_push_scope(t, &new_ctx);
   elab_generics(t, t, &new_ctx);
   elab_ports(t, t, &new_ctx);
   elab_decls(t, &new_ctx);

   if (error_count() == base_errors) {
      elab_lower(b, NULL, &new_ctx);
      elab_stmts(t, &new_ctx);
   }

   elab_pop_scope(&new_ctx);
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

static void elab_top_level_generics(tree_t arch, elab_ctx_t *ctx)
{
   tree_t ent = tree_primary(arch);
   const int ngenerics = tree_generics(ent);

   if (ctx->generics == NULL && ngenerics > 0)
      ctx->generics = hash_new(ngenerics * 2);

   for (int i = 0; i < ngenerics; i++) {
      tree_t g = tree_generic(ent, i);
      ident_t name = tree_ident(g);

      tree_t value = elab_find_generic_override(g, ctx);
      if (value == NULL && tree_has_value(g))
         value = tree_value(g);
      else if (value == NULL) {
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

      if (is_literal(value))
         hash_put(ctx->generics, g, value);
   }
}

static void elab_top_level(tree_t arch, ident_t ename, const elab_ctx_t *ctx)
{
   const char *name = simple_name(istr(tree_ident2(arch)));
   ident_t ninst = hpathf(ctx->inst_name, ':', ":%s(%s)", name,
                          simple_name(istr(tree_ident(arch))));
   ident_t npath = hpathf(ctx->path_name, ':', ":%s", name);
   ident_t ndotted = ident_prefix(ctx->dotted, ename, '.');

   tree_t b = tree_new(T_BLOCK);
   tree_set_ident(b, ename);
   tree_set_loc(b, tree_loc(arch));

   tree_add_stmt(ctx->out, b);

   elab_ctx_t new_ctx = {
      .out       = b,
      .path_name = npath,
      .inst_name = ninst,
      .dotted    = ndotted,
   };
   elab_inherit_context(&new_ctx, ctx);
   elab_subprogram_prefix(arch, &new_ctx);

   tree_t arch_copy = elab_copy(arch, &new_ctx);
   tree_t entity = tree_primary(arch_copy);

   elab_push_scope(arch, &new_ctx);
   elab_context(entity);
   elab_context(arch_copy);
   elab_top_level_generics(arch_copy, &new_ctx);
   elab_top_level_ports(entity, &new_ctx);
   elab_decls(entity, &new_ctx);

   simplify_global(arch_copy, new_ctx.generics, ctx->jit, ctx->registry);

   new_ctx.drivers = find_drivers(arch_copy);

   if (error_count() == 0)
      elab_decls(arch_copy, &new_ctx);

   if (error_count() == 0) {
      elab_lower(b, NULL, &new_ctx);
      elab_stmts(entity, &new_ctx);
      elab_stmts(arch_copy, &new_ctx);
   }

   elab_pop_scope(&new_ctx);
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
   new->next  = generic_override;

   generic_override = new;
}

tree_t elab(object_t *top, jit_t *jit, unit_registry_t *ur, cover_data_t *cover)
{
   make_new_arena();

   ident_t name = NULL;

   tree_t vhdl = tree_from_object(top);
   if (vhdl != NULL)
      name = ident_prefix(tree_ident(vhdl), well_known(W_ELAB), '.');

   vlog_node_t vlog = vlog_from_object(top);
   if (vlog != NULL)
      name = ident_prefix(vlog_ident(vlog), well_known(W_ELAB), '.');

   if (vhdl == NULL && vlog == NULL)
      fatal("top level is not a VHDL design unit or Verilog module");

   tree_t e = tree_new(T_ELAB);
   tree_set_ident(e, name);
   tree_set_loc(e, &(top->loc));

   lib_t work = lib_work();

   elab_ctx_t ctx = {
      .out       = e,
      .root      = e,
      .path_name = NULL,
      .inst_name = NULL,
      .cover     = cover,
      .library   = work,
      .jit       = jit,
      .registry  = ur,
      .shapes    = hash_new(16),
      .dotted    = lib_name(work),
   };

   if (vhdl != NULL) {
      switch (tree_kind(vhdl)) {
      case T_ENTITY:
         {
            tree_t arch = elab_pick_arch(&(top->loc), vhdl, &ctx);
            elab_top_level(arch, tree_ident2(arch), &ctx);
         }
         break;
      case T_ARCH:
         elab_top_level(vhdl, tree_ident2(vhdl), &ctx);
         break;
      case T_CONFIGURATION:
         {
            tree_t arch = elab_root_config(vhdl, &ctx);
            elab_top_level(arch, tree_ident2(arch), &ctx);
         }
         break;
      default:
         fatal("%s is not a suitable top-level unit", istr(tree_ident(vhdl)));
      }
   }
   else {
      tree_t wrap = tree_new(T_VERILOG);
      tree_set_loc(wrap, vlog_loc(vlog));
      tree_set_ident(wrap, vlog_ident(vlog));
      tree_set_vlog(wrap, vlog);

      elab_verilog_module(wrap, &ctx);
   }

   hash_free(ctx.shapes);

   if (error_count() > 0)
      return NULL;

   if (opt_get_verbose(OPT_ELAB_VERBOSE, NULL))
      dump(e);

   for (generic_list_t *it = generic_override; it != NULL; it = it->next)
      warnf("generic value for %s not used", istr(it->name));

   ident_t b0_name = tree_ident(tree_stmt(e, 0));
   ident_t vu_name = ident_prefix(lib_name(ctx.library), b0_name, '.');
   unit_registry_flush(ur, vu_name);

   freeze_global_arena();

   lib_put(ctx.library, e);

   return e;
}

tree_t elab_external_name(tree_t name, tree_t root, ident_t *path)
{
   tree_t where = root, next = NULL;
   const int nparts = tree_parts(name);

   for (int i = 0; i < nparts; i++, where = next, next = NULL) {
      tree_t pe = tree_part(name, i);
      ident_t id = NULL;

      switch (tree_subkind(pe)) {
      case PE_ABSOLUTE:
         {
            tree_t entity = tree_part(name, ++i);
            assert(tree_subkind(entity) == PE_SIMPLE);

            if (root == NULL || tree_ident(root) != tree_ident(entity)) {
               error_at(tree_loc(entity), "%s is not the name of the root of "
                        "the design hierarchy", istr(tree_ident(entity)));
               return NULL;
            }

            next = root;
         }
         continue;
      case PE_SIMPLE:
         id = tree_ident(pe);
         break;
      case PE_GENERATE:
         {
            LOCAL_TEXT_BUF tb = tb_new();
            tb_istr(tb, tree_ident(pe));
            tb_printf(tb, "(%"PRIi64")", assume_int(tree_value(pe)));

            id = ident_new(tb_get(tb));
         }
         break;
      case PE_RELATIVE:
         next = where;
         continue;
      case PE_CARET:
         if ((next = where) == NULL) {
            error_at(tree_loc(pe), "relative pathname has no containing "
                     "declarative region");
            return NULL;
         }
         continue;
      default:
         error_at(tree_loc(pe), "sorry, this form of external name is not "
                  "yet supported");
         return NULL;
      }

      if (!is_concurrent_block(where)) {
         diag_t *d = diag_new(DIAG_ERROR, tree_loc(pe));
         diag_printf(d, "%s is not a concurrent region",
                     istr(tree_ident(where)));
         diag_hint(d, tree_loc(where), "location of %s",
                   istr(tree_ident(where)));
         diag_emit(d);
         return NULL;
      }

      tree_t hier = tree_decl(where, 0);
      assert(tree_kind(hier) == T_HIER);

      if (tree_subkind(hier) == T_COMPONENT) {
         // Skip over implicit block for component declaration
         where = tree_stmt(where, 0);
         assert(tree_kind(where) == T_BLOCK);
         hier = tree_decl(where, 0);
         *path = ident_prefix(*path, tree_ident(where), '.');
      }

      const int nstmts = tree_stmts(where);
      for (int i = 0; i < nstmts; i++) {
         tree_t s = tree_stmt(where, i);
         if (tree_ident(s) == id) {
            next = s;
            break;
         }
      }

      if (next == NULL)
         next = search_decls(where, id, 0);

      if (next == NULL) {
         diag_t *d = diag_new(DIAG_ERROR, tree_loc(pe));
         diag_printf(d, "external name %s not found",
                     istr(tree_ident(tree_part(name, nparts - 1))));
         diag_hint(d, tree_loc(pe), "name %s not found inside %s", istr(id),
                   istr(tree_ident(where)));

         if (tree_kind(where) == T_BLOCK) {
            tree_t unit = tree_ref(hier);
            const int nstmts = tree_stmts(unit);
            for (int i = 0; i < nstmts; i++) {
               tree_t s = tree_stmt(unit, i);
               if (tree_ident(s) == id) {
                  diag_hint(d, NULL, "an object cannot be referenced by an "
                            "external name until it has been elaborated");
                  break;
               }
            }
         }

         diag_emit(d);
         return NULL;
      }

      if (i + 1 < nparts)
         *path = ident_prefix(*path, id, '.');
   }

   if (class_of(where) != tree_class(name)) {
      diag_t *d = diag_new(DIAG_ERROR, tree_loc(name));
      diag_printf(d, "class of object %s is not %s", istr(tree_ident(where)),
                  class_str(tree_class(name)));
      diag_hint(d, tree_loc(name), "external name with class %s",
                class_str(tree_class(name)));
      diag_hint(d, tree_loc(where), "declaration of %s as %s",
                istr(tree_ident(where)), class_str(class_of(where)));
      diag_emit(d);
      return NULL;
   }
   else if (!type_eq(tree_type(where), tree_type(name))) {
      diag_t *d = diag_new(DIAG_ERROR, tree_loc(name));
      diag_printf(d, "type of %s %s is not %s",
                  class_str(tree_class(name)), istr(tree_ident(where)),
                  type_pp(tree_type(name)));
      diag_hint(d, tree_loc(name), "external name with type %s",
                type_pp(tree_type(name)));
      diag_hint(d, tree_loc(where), "declaration of %s with type %s",
                istr(tree_ident(where)), type_pp(tree_type(where)));
      diag_emit(d);
      return NULL;
   }

   return where;
}

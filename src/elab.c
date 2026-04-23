//
//  Copyright (C) 2011-2026  Nick Gasson
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
#include "hier.h"
#include "eval.h"
#include "hash.h"
#include "inst.h"
#include "lib.h"
#include "lower.h"
#include "mask.h"
#include "object.h"
#include "option.h"
#include "phase.h"
#include "printf.h"
#include "psl/psl-phase.h"
#include "rt/model.h"
#include "rt/structs.h"
#include "thread.h"
#include "type.h"
#include "vhdl/vhdl-phase.h"
#include "vlog/vlog-defs.h"
#include "vlog/vlog-node.h"
#include "vlog/vlog-phase.h"
#include "vlog/vlog-util.h"

#include <assert.h>
#include <stdarg.h>
#include <stdlib.h>
#include <inttypes.h>

#define MAX_DEPTH 127    // Limited by vcode type indexes

typedef A(tree_t) tree_list_t;

typedef struct {
   vlog_node_t body;
   ident_t     dotted;
   vlog_node_t parent_body;
   ident_t     parent_dotted;
   ident_t     inst_alias;
} vlog_deferred_body_t;
typedef A(vlog_deferred_body_t) vlog_deferred_list_t;

typedef struct _elab_ctx elab_ctx_t;
typedef struct _generic_list generic_list_t;

typedef struct _elab_ctx {
   const elab_ctx_t *parent;
   tree_t            out;
   object_t         *root;
   tree_t            inst;
   ident_t           dotted;
   ident_t           cloned;
   ident_t           inst_alias;   // parent.label alias for module
                                   // instances (NULL for generate blks)
   lib_t             library;
   jit_t            *jit;
   unit_registry_t  *registry;
   mir_context_t    *mir;
   lower_unit_t     *lowered;
   cover_data_t     *cover;
   sdf_file_t       *sdf;
   hash_t           *modcache;
   hash_t           *blockcache;  // ident_t dotted → vlog_node_t body
   hash_t           *mracache;
   hash_t           *scope_tree;  // ident_t dotted → hier_node_t*
                                  // (every elaborated scope, both
                                  // VHDL and Verilog; lives on the
                                  // synthetic root)
   rt_model_t       *model;
   rt_scope_t       *scope;
   mem_pool_t       *pool;
   cover_scope_t    *cscope;
   vlog_node_t          vlog_body;     // V_INST_BODY / V_BLOCK / generate
                                       // body for hier-ref resolver
   vlog_deferred_list_t *vlog_deferred; // Accumulated bodies for
                                        // post-elab hier-ref resolution
   unsigned          depth;
   unsigned          errors;
   bool              is_synthetic_root;   // anonymous anchor above tops
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

typedef struct {
   object_t *object;
   ghash_t  *instances;
   unsigned  count;
   unsigned  unique;
} mod_cache_t;

typedef struct {
   vlog_node_t    body;
   tree_t         block;
   tree_t         wrap;
   cover_scope_t *cscope;
} elab_instance_t;

static void elab_vhdl_block(tree_t t, const elab_ctx_t *ctx);
static void elab_vhdl_processes(tree_t t, const elab_ctx_t *ctx);
static void elab_vhdl_sub_blocks(tree_t t, const elab_ctx_t *ctx);
static void elab_verilog_sub_blocks(vlog_node_t v, const elab_ctx_t *ctx);
static void elab_verilog_processes(vlog_node_t v, const elab_ctx_t *ctx);
static void elab_decls(tree_t t, const elab_ctx_t *ctx);
static void elab_push_scope(tree_t t, elab_ctx_t *ctx);
static void elab_pop_scope(elab_ctx_t *ctx);
static void elab_resolve_all_vlog_hier_refs(const elab_ctx_t *ctx);

static generic_list_t *generic_override = NULL;

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

static tree_t elab_pick_arch(tree_t entity, const loc_t *loc,
                             const elab_ctx_t *ctx)
{
   // When an explicit architecture name is not given select the most
   // recently analysed architecture of this entity

   tree_t cached = hash_get(ctx->mracache, entity);
   if (cached != NULL)
      return cached;

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

   tree_t arch = lib_get(lib, params.chosen);
   hash_put(ctx->mracache, entity, arch);
   return arch;
}

static uint32_t elab_hash_vhdl_generic(tree_t g, tree_t value)
{
   uint32_t h = ident_hash(tree_ident(g));

   switch (tree_class(g)) {
   case C_TYPE:
      h ^= (uintptr_t)tree_type(value);
      break;
   case C_FUNCTION:
   case C_PROCEDURE:
   case C_PACKAGE:
      h ^= (uintptr_t)tree_ref(value);
      break;
   case C_CONSTANT:
      // Only literal values are folded so instances with different
      // non-literal generic maps can be reused
      switch (tree_kind(value)) {
      case T_REF:
         {
            tree_t decl = tree_ref(value);
            if (tree_kind(decl) == T_ENUM_LIT)
               h ^= knuth_hash(tree_pos(decl));
         }
         break;
      case T_LITERAL:
         switch (tree_subkind(value)) {
         case L_PHYSICAL:
            h ^= ident_hash(tree_ident(value));
            // Fall-through
         case L_INT:
            h ^= mix_bits_64(tree_ival(value));
            break;
         case L_REAL:
            h ^= mix_bits_64(FLOAT_BITS(tree_dval(value)));
            break;
         default:
            should_not_reach_here();
         }
         break;
      default:
         break;
      }
      break;
   default:
      should_not_reach_here();
   }

   return h;
}

static uint32_t elab_hash_vhdl_inst(tree_t t)
{
   uint32_t h = 0;

   switch (tree_kind(t)) {
   case T_SPEC:
      {
         if (tree_has_value(t)) {
            tree_t bind = tree_value(t);
            h ^= elab_hash_vhdl_inst(bind);

            // Mapping of component to entity ports is significant
            const int nparams = tree_params(bind);
            for (int i = 0; i < nparams; i++) {
               tree_t m = tree_param(bind, i);
               if (tree_subkind(m) == P_POS) {
                  tree_t value = tree_value(m);
                  if (tree_kind(value) == T_REF)
                     h ^= (uintptr_t)tree_ref(value);
                  else
                     h ^= (uintptr_t)value;
               }
               else
                  h ^= (uintptr_t)m;   // Ignore for now
            }
         }

         if (tree_decls(t) > 0)
            h ^= (uintptr_t)t;   // Ignore for now
      }
      break;
   case T_INSTANCE:
      if (tree_has_spec(t))
         h ^= elab_hash_vhdl_inst(tree_spec(t));
      // Fall-through
   case T_BINDING:
      {
         tree_t unit = tree_ref(t), ent = primary_unit_of(unit);
         h ^= mix_bits_64((uintptr_t)unit);

         const int ngenmaps = tree_genmaps(t);
         for (int i = 0; i < ngenmaps; i++) {
            tree_t m = tree_genmap(t, i);
            assert(tree_subkind(m) == P_POS);

            h ^= elab_hash_vhdl_generic(tree_generic(ent, i), tree_value(m));
         }
      }
      break;
   default:
      should_not_reach_here();
   }

   return h;
}

static uint32_t elab_hash_inst(const void *key)
{
   tree_t t = tree_from_object((object_t *)key);
   if (t != NULL)
      return elab_hash_vhdl_inst(t);

   vlog_node_t v = vlog_from_object((object_t *)key);
   if (v != NULL) {
      assert(vlog_kind(v) == V_INST_LIST);

      uint32_t h = ident_hash(vlog_ident(v));

      const int nparams = vlog_params(v);
      for (int i = 0; i < nparams; i++)
         h ^= vlog_hash_node(vlog_param(v, i));

      return h;
   }

   should_not_reach_here();
}

static bool elab_cmp_vhdl_generic(tree_t g, tree_t a, tree_t b)
{
   switch (tree_class(g)) {
   case C_CONSTANT:
      return (!is_literal(a) && !is_literal(b)) || same_tree(a, b);
   case C_TYPE:
      return type_strict_eq(tree_type(a), tree_type(b));
   case C_FUNCTION:
   case C_PROCEDURE:
   case C_PACKAGE:
      return tree_ref(a) == tree_ref(b);
   default:
      should_not_reach_here();
   }
}

static bool elab_cmp_vhdl_inst(tree_t a, tree_t b)
{
   const tree_kind_t kind = tree_kind(a);
   if (kind != tree_kind(b))
      return false;

   switch (kind) {
   case T_SPEC:
      {
         tree_t a_bind = tree_has_value(a) ? tree_value(a) : NULL;
         tree_t b_bind = tree_has_value(b) ? tree_value(b) : NULL;

         if (a_bind != NULL && b_bind != NULL) {
            if (!elab_cmp_vhdl_inst(a_bind, b_bind))
               return false;

            const int nparams = tree_params(a_bind);
            if (tree_params(b_bind) != nparams)
               return false;

            for (int i = 0; i < nparams; i++) {
               tree_t ma = tree_param(a_bind, i);
               tree_t mb = tree_param(b_bind, i);

               if (tree_subkind(ma) != P_POS || tree_subkind(mb) != P_POS)
                  return false;

               tree_t a_value = tree_value(ma);
               tree_t b_value = tree_value(mb);

               if (tree_kind(a_value) != T_REF || tree_kind(b_value) != T_REF)
                  return false;
               else if (tree_ref(a_value) != tree_ref(b_value))
                  return false;
            }
         }
         else if (a_bind != NULL || b_bind != NULL)
            return false;

         if (tree_decls(a) > 0 || tree_decls(b) > 0)
            return false;

         return true;
      }
   case T_INSTANCE:
      {
         tree_t a_spec = tree_has_spec(a) ? tree_spec(a) : NULL;
         tree_t b_spec = tree_has_spec(b) ? tree_spec(b) : NULL;

         if (a_spec != NULL && b_spec != NULL) {
            if (!elab_cmp_vhdl_inst(tree_spec(a), tree_spec(b)))
               return false;
         }
         else if (a_spec != NULL || b_spec != NULL)
            return false;
      }
      // Fall-through
   case T_BINDING:
      {
         tree_t unit = tree_ref(a), ent = primary_unit_of(unit);
         if (tree_ref(b) != unit)
            return false;

         const int ngenmaps = tree_genmaps(a);
         if (tree_genmaps(b) != ngenmaps)
            return false;

         for (int i = 0; i < ngenmaps; i++) {
            tree_t ma = tree_genmap(a, i);
            tree_t mb = tree_genmap(b, i);

            assert(tree_subkind(ma) == P_POS);
            assert(tree_subkind(mb) == P_POS);

            tree_t a = tree_value(ma);
            tree_t b = tree_value(mb);

            if (!elab_cmp_vhdl_generic(tree_generic(ent, i), a, b))
               return false;
         }

         return true;
      }
   default:
      should_not_reach_here();
   }
}

static bool elab_cmp_inst(const void *a, const void *b)
{
   tree_t ta = tree_from_object((object_t *)a);
   tree_t tb = tree_from_object((object_t *)b);
   if (ta != NULL && tb != NULL)
      return elab_cmp_vhdl_inst(ta, tb);

   vlog_node_t va = vlog_from_object((object_t *)a);
   vlog_node_t vb = vlog_from_object((object_t *)b);
   if (va != NULL && vb != NULL) {
      assert(vlog_kind(va) == V_INST_LIST);
      assert(vlog_kind(vb) == V_INST_LIST);

      if (vlog_ident(va) != vlog_ident(vb))
         return false;

      const int nparams = vlog_params(va);
      if (vlog_params(vb) != nparams)
         return false;

      for (int i = 0; i < nparams; i++) {
         if (!vlog_equal_node(vlog_param(va, i), vlog_param(vb, i)))
            return false;
      }

      return true;
   }

   return false;
}

static mod_cache_t *elab_cached_module(object_t *obj, const elab_ctx_t *ctx)
{
   mod_cache_t *mc = hash_get(ctx->modcache, obj);
   if (mc != NULL)
      return mc;

   mc = pool_calloc(ctx->pool, sizeof(mod_cache_t));
   mc->object    = obj;
   mc->instances = ghash_new(4, elab_hash_inst, elab_cmp_inst);

   hash_put(ctx->modcache, obj, mc);
   return mc;
}

static bool elab_synth_binding_cb(lib_t lib, void *__ctx)
{
   synth_binding_params_t *params = __ctx;

   ident_t name = ident_prefix(lib_name(lib), tree_ident(params->comp), '.');
   *(params->result) = lib_get(lib, name);

   return *(params->result) == NULL;
}

static tree_t elab_to_vhdl(type_t from, type_t to)
{
   static struct {
      const verilog_type_t from_id;
      const ieee_type_t    to_id;
      const char *const    func;
      type_t               from;
      type_t               to;
      tree_t               decl;
   } table[] = {
      { VERILOG_LOGIC, IEEE_STD_LOGIC, "NVC.VERILOG.TO_VHDL(" T_LOGIC ")U" },
      { VERILOG_NET_VALUE, IEEE_STD_LOGIC,
        "NVC.VERILOG.TO_VHDL(" T_NET_VALUE ")U" },
      { VERILOG_WIRE_ARRAY, IEEE_STD_ULOGIC_VECTOR,
        "NVC.VERILOG.TO_VHDL(" T_WIRE_ARRAY ")Y" },
      { VERILOG_WIRE_ARRAY, IEEE_STD_LOGIC_VECTOR,
        "NVC.VERILOG.TO_VHDL(" T_WIRE_ARRAY ")Y" },
      { VERILOG_LOGIC_ARRAY, IEEE_STD_ULOGIC_VECTOR,
        "NVC.VERILOG.TO_VHDL(" T_LOGIC_ARRAY ")Y" },
      { VERILOG_LOGIC_ARRAY, IEEE_STD_LOGIC_VECTOR,
        "NVC.VERILOG.TO_VHDL(" T_LOGIC_ARRAY ")Y" },
   };

   INIT_ONCE({
         for (int i = 0; i < ARRAY_LEN(table); i++) {
            table[i].from = verilog_type(table[i].from_id);
            table[i].to   = ieee_type(table[i].to_id);
            table[i].decl = verilog_func(ident_new(table[i].func));
         }
      });

   for (int i = 0; i < ARRAY_LEN(table); i++) {
      if (type_eq(table[i].from, from) && type_eq(table[i].to, to))
         return table[i].decl;
   }

   return NULL;
}

static tree_t elab_to_verilog(type_t from, type_t to)
{
   static struct {
      const ieee_type_t    from_id;
      const verilog_type_t to_id;
      const char *const    func;
      type_t               from;
      type_t               to;
      tree_t               decl;
   } table[] = {
      { IEEE_STD_ULOGIC, VERILOG_LOGIC, "NVC.VERILOG.TO_VERILOG(U)" T_LOGIC },
      { IEEE_STD_ULOGIC, VERILOG_NET_VALUE,
        "NVC.VERILOG.TO_VERILOG(U)" T_NET_VALUE },
      { IEEE_STD_ULOGIC_VECTOR, VERILOG_WIRE_ARRAY,
        "NVC.VERILOG.TO_VERILOG(Y)" T_NET_ARRAY },
      { IEEE_STD_LOGIC_VECTOR, VERILOG_WIRE_ARRAY,
        "NVC.VERILOG.TO_VERILOG(Y)" T_NET_ARRAY },
   };

   INIT_ONCE({
         for (int i = 0; i < ARRAY_LEN(table); i++) {
           table[i].from = ieee_type(table[i].from_id);
           table[i].to   = verilog_type(table[i].to_id);
           table[i].decl = verilog_func(ident_new(table[i].func));
         }
      });

   for (int i = 0; i < ARRAY_LEN(table); i++) {
      if (type_eq(table[i].from, from) && type_eq(table[i].to, to))
         return table[i].decl;
   }

   return NULL;
}

static tree_t elab_mixed_binding(tree_t comp, const elab_instance_t *ei)
{
   assert(tree_kind(comp) == T_COMPONENT);

   tree_t bind = tree_new(T_BINDING);
   tree_set_ident(bind, tree_ident(ei->wrap));
   tree_set_loc(bind, tree_loc(comp));
   tree_set_ref(bind, ei->wrap);
   tree_set_class(bind, C_ENTITY);

   const int vhdl_nports = tree_ports(comp);
   const int vlog_nports = vlog_ports(ei->body);

   LOCAL_BIT_MASK have;
   mask_init(&have, vhdl_nports);

   bool have_named = false;
   for (int i = 0; i < vlog_nports; i++) {
      vlog_node_t ref = vlog_port(ei->body, i);
      assert(vlog_kind(ref) == V_REF);

      vlog_node_t mport = vlog_ref(ref);
      assert(vlog_kind(mport) == V_PORT_DECL);

      ident_t name = vlog_ident2(mport);

      tree_t vport = tree_port(ei->block, i);
      assert(tree_ident(vport) == vlog_ident(mport));

      tree_t cport = NULL;
      for (int j = 0; j < vhdl_nports; j++) {
         tree_t pj = tree_port(comp, j);
         if (ident_casecmp(tree_ident(pj), name)) {
            cport = pj;
            mask_set(&have, j);
            break;
         }
      }

      if (cport == NULL) {
         error_at(tree_loc(comp), "missing matching VHDL port declaration for "
                  "Verilog port '%s' in component %s", istr(vlog_ident(mport)),
                  istr(tree_ident(comp)));
         return NULL;
      }

      const v_port_kind_t mdir = vlog_subkind(mport);
      const port_mode_t cdir = tree_subkind(cport);

      if (mdir != V_PORT_INPUT && mdir != V_PORT_OUTPUT) {
         error_at(tree_loc(cport), "only input and output ports are supported "
                  "for mixed language instantiation");
         return NULL;
      }
      else if (!(mdir == V_PORT_INPUT && cdir == PORT_IN)
               && !(mdir == V_PORT_OUTPUT && cdir == PORT_OUT)) {
         error_at(tree_loc(cport), "VHDL port direction %s does not match "
                  "corresponding Verilog port '%s' which is declared as %s",
                  port_mode_str(cdir), istr(vlog_ident(mport)),
                  mdir == V_PORT_INPUT ? "input" : "output");
         return NULL;
      }

      type_t btype = tree_type(cport);
      type_t vtype = tree_type(vport);

      tree_t conv = tree_new(T_CONV_FUNC);
      tree_set_loc(conv, tree_loc(cport));

      if (mdir == V_PORT_INPUT) {
         tree_t func = elab_to_verilog(btype, vtype);
         if (func == NULL) {
            error_at(tree_loc(cport), "cannot connect VHDL signal with type "
                     "%s to Verilog input port '%s'", type_pp(btype),
                     istr(vlog_ident(mport)));
            return NULL;
         }

         tree_set_ref(conv, func);
         tree_set_ident(conv, tree_ident(func));
         tree_set_type(conv, type_result(tree_type(func)));
         tree_set_value(conv, make_ref(cport));

         if (have_named)
            add_param(bind, conv, P_NAMED, make_ref(vport));
         else
            add_param(bind, conv, P_POS, NULL);
      }
      else {
         tree_t func = elab_to_vhdl(vtype, btype);
         if (func == NULL) {
            error_at(tree_loc(cport), "cannot connect VHDL signal with type "
                     "%s to Verilog output port '%s'", type_pp(btype),
                     istr(vlog_ident(mport)));
            return NULL;
         }

         tree_set_ref(conv, func);
         tree_set_ident(conv, tree_ident(func));
         tree_set_type(conv, type_result(tree_type(func)));
         tree_set_value(conv, make_ref(vport));

         add_param(bind, make_ref(cport), P_NAMED, conv);
         have_named = true;
      }
   }

   for (int i = 0; i < vhdl_nports; i++) {
      if (!mask_test(&have, i)) {
         tree_t p = tree_port(comp, i);
         diag_t *d = diag_new(DIAG_ERROR, tree_loc(p));
         diag_printf(d, "port %s not found in Verilog module %s",
                     istr(tree_ident(p)), istr(vlog_ident2(ei->body)));
         diag_emit(d);
      }
   }

   return bind;
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

   object_t *obj = lib_get_generic(lib, full_i, NULL);

   if (vlog_from_object(obj) != NULL)
      return NULL;  // Verilog binding handled separately

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

   tree_t arch = elab_pick_arch(entity, tree_loc(comp), ctx);

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
         if (ident_casecmp(tree_ident(eg), tree_ident(cg))) {
            match = cg;
            break;
         }
      }

      tree_t value;
      if (match != NULL) {
         const class_t class = tree_class(eg);

         if (class != tree_class(match)) {
            diag_t *d = diag_new(DIAG_ERROR, tree_loc(inst));
            diag_printf(d, "generic %s in component %s has class %s which is "
                        "incompatible with class %s in entity %s",
                        istr(tree_ident(match)), istr(tree_ident(comp)),
                        class_str(tree_class(match)), class_str(class),
                        istr(tree_ident(entity)));
            diag_hint(d, tree_loc(match), "declaration of generic %s in "
                      "component", istr(tree_ident(match)));
            diag_hint(d, tree_loc(eg), "declaration of generic %s in entity",
                      istr(tree_ident(eg)));
            diag_emit(d);
            return NULL;
         }
         else if (class == C_PACKAGE) {
            value = tree_new(T_REF);
            tree_set_ident(value, tree_ident(match));
            tree_set_ref(value, match);
         }
         else {
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
      }
      else if (tree_has_value(eg)) {
         tree_t def = tree_value(eg);
         if (is_literal(def))
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
         if (ident_casecmp(tree_ident(ep), tree_ident(cp))) {
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

static void elab_write_generic(text_buf_t *tb, tree_t value)
{
   switch (tree_kind(value)) {
   case T_LITERAL:
      switch (tree_subkind(value)) {
      case L_INT:  tb_printf(tb, "%"PRIi64, tree_ival(value)); break;
      case L_REAL: tb_printf(tb, "%lf", tree_dval(value)); break;
      case L_PHYSICAL:
         tb_printf(tb, "%"PRIi64" %s", tree_ival(value),
                   istr(tree_ident(value)));
         break;
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
      if (is_subprogram(tree_ref(value)))
         tb_printf(tb, "%s", type_pp(tree_type(value)));
      else
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

         tree_t open = tree_new(T_OPEN);
         tree_set_type(open, tree_type(p));
         tree_set_loc(open, tree_loc(p));

         tree_set_value(map, open);

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
      tree_set_ident(result, tree_ident(type_unit(type, 0)));

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

   ident_t name = tree_ident(g);

   generic_list_t **it, *tmp;
   for (it = &generic_override;
        *it && !ident_casecmp((*it)->name, name);
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

static tree_t elab_override_instance_generics(tree_t t, const elab_ctx_t *ctx)
{
   if (generic_override == NULL)
      return t;

   ident_t prefix = tree_ident(t);
   for (const elab_ctx_t *e = ctx; e->inst; e = e->parent)
      prefix = ident_prefix(tree_ident(e->inst), prefix, '.');

   tree_t unit = tree_ref(t), entity = primary_unit_of(unit);

   const int ngenerics = tree_generics(entity);
   assert(tree_genmaps(t) == ngenerics);

   tree_t new = tree_new(T_INSTANCE);
   tree_set_loc(new, tree_loc(t));
   tree_set_ref(new, unit);
   tree_set_class(new, tree_class(t));
   tree_set_ident(new, tree_ident(t));
   tree_set_ident2(new, tree_ident2(t));

   for (int i = 0; i < ngenerics; i++) {
      tree_t g = tree_generic(entity, i);
      tree_t m = tree_genmap(t, i);
      assert(tree_subkind(m) == P_POS);
      assert(tree_pos(m) == i);

      ident_t qual = ident_prefix(prefix, tree_ident(g), '.');

      generic_list_t **it, *tmp;
      for (it = &generic_override;
           *it && !ident_casecmp((*it)->name, qual);
           it = &((*it)->next))
         ;

      if (*it == NULL)
         tree_add_genmap(new, m);
      else {
         tree_t m2 = tree_new(T_PARAM);
         tree_set_subkind(m2, P_POS);
         tree_set_pos(m2, i);
         tree_set_value(m2, elab_parse_generic_string(g, (*it)->value));

         tree_add_genmap(new, m2);

         *it = (tmp = (*it))->next;
         free(tmp->value);
         free(tmp);
      }
   }

   const int nparams = tree_params(t);
   for (int i = 0; i < nparams; i++)
      tree_add_param(new, tree_param(t, i));

   return new;
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

      if (map == NULL) {
         error_at(tree_loc(bind), "missing value for generic %s with no "
                  "default", istr(tree_ident(g)));
         continue;
      }

      tree_add_genmap(ctx->out, map);
   }
}

static void elab_map_generic_type(type_t generic, type_t actual, hash_t *map)
{
   assert(type_kind(generic) == T_GENERIC);

   switch (type_subkind(generic)) {
   case GTYPE_ARRAY:
      {
         type_t gelem = type_elem(generic);
         if (type_kind(gelem) == T_GENERIC && !type_has_ident(gelem))
            elab_map_generic_type(gelem, type_elem(actual), map);

         const int ndims = type_indexes(generic);
         for (int i = 0; i < ndims; i++) {
            type_t index = type_index(generic, i);
            if (type_kind(index) == T_GENERIC && !type_has_ident(index))
               elab_map_generic_type(index, index_type_of(actual, i), map);
         }
      }
      break;
   }

   hash_put(map, generic, actual);
}

static void elab_map_generic_package(tree_t generic, tree_t actual, hash_t *map)
{
   tree_t formal = tree_ref(tree_value(generic));

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
   for (int i = 0; i < ngenerics; i++) {
      tree_t fg = tree_generic(formal, i);
      tree_t ag = tree_generic(actual, i);

      tree_t m = tree_genmap(actual, i);
      assert(tree_subkind(m) == P_POS);

      tree_t value = tree_value(m);

      switch (tree_class(fg)) {
      case C_FUNCTION:
      case C_PROCEDURE:
         // Get the actual subprogram from the generic map
         assert(tree_kind(value) == T_REF);
         hash_put(map, fg, tree_ref(value));
         break;
      case C_TYPE:
         elab_map_generic_type(tree_type(fg), tree_type(ag), map);
         break;
      case C_CONSTANT:
         if (is_literal(value)) {
            hash_put(map, fg, value);
            hash_put(map, ag, value);
         }
         else if (tree_kind(value) == T_REF) {
            tree_t ref = tree_ref(value);
            hash_put(map, fg, ref);
            hash_put(map, ag, ref);
         }
         else {
            hash_put(map, fg, ag);
         }
         break;
      case C_PACKAGE:
         // TODO: this should be processed recursively
      default:
         hash_put(map, fg, ag);
         break;
      }
   }

   hash_put(map, generic, actual);
}

static void elab_fold_generics(tree_t b, const elab_ctx_t *ctx)
{
   assert(tree_kind(b) == T_BLOCK);

   const int ngenerics = tree_generics(b);
   assert(tree_genmaps(b) == ngenerics);

   hash_t *map = NULL, *fixup = NULL;

   for (int i = 0; i < ngenerics; i++) {
      tree_t g = tree_generic(b, i);

      tree_t m = tree_genmap(b, i);
      assert(tree_subkind(m) == P_POS);

      tree_t value = tree_value(m);

      const class_t class = tree_class(g);
      if (fixup == NULL && class != C_CONSTANT)
         fixup = hash_new(ngenerics * 2);
      else if (map == NULL && class == C_CONSTANT)
         map = hash_new(ngenerics * 2);

      switch (class) {
      case C_CONSTANT:
         if (is_literal(value)) {
            // These values can be safely substituted for all references
            // to the generic name
            hash_put(map, g, value);
         }
         break;

      case C_TYPE:
         elab_map_generic_type(tree_type(g), tree_type(value), fixup);
         break;

      case C_PACKAGE:
         elab_map_generic_package(g, tree_ref(value), fixup);
         break;

      case C_FUNCTION:
      case C_PROCEDURE:
         hash_put(fixup, g, tree_ref(value));
         break;

      default:
         should_not_reach_here();
      }
   }

   if (fixup != NULL)
      instance_fixup(b, fixup);

   simplify_global(b, map, ctx->jit, ctx->registry, ctx->mir);

   hash_free(map);
   hash_free(fixup);
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

// True when `parent` is either absent (we are bootstrapping) or is
// the synthetic anonymous root.  In either case the caller's ctx is
// a top-level design scope and should not track an enclosing instance
// for diagnostics.
static inline bool elab_parent_is_root(const elab_ctx_t *parent)
{
   return parent == NULL || parent->is_synthetic_root;
}

// Thin wrapper around hier_scope_alias() for this module's ctx type.
// The rule lives in hier.h; we just project the fields.
static inline ident_t vlog_scope_alias(const elab_ctx_t *c)
{
   hier_scope_t s = { c->inst_alias, c->cloned, c->dotted };
   return hier_scope_alias(&s);
}

static void elab_inherit_context(elab_ctx_t *ctx, const elab_ctx_t *parent)
{
   ctx->parent         = parent;
   ctx->jit            = parent->jit;
   ctx->registry       = parent->registry;
   ctx->mir            = parent->mir;
   ctx->root           = parent->root;
   ctx->dotted         = ctx->dotted ?: parent->dotted;
   ctx->library        = ctx->library ?: parent->library;
   ctx->out            = ctx->out ?: parent->out;
   ctx->cover          = parent->cover;
   ctx->sdf            = parent->sdf;
   ctx->inst           = ctx->inst ?: parent->inst;
   ctx->modcache       = parent->modcache;
   ctx->blockcache     = parent->blockcache;
   ctx->mracache       = parent->mracache;
   ctx->scope_tree     = parent->scope_tree;
   ctx->depth          = parent->depth + 1;
   ctx->model          = parent->model;
   ctx->errors         = error_count();
   ctx->pool           = parent->pool;
   ctx->cscope         = parent->cscope;
   ctx->vlog_body      = ctx->vlog_body ?: parent->vlog_body;
   ctx->vlog_deferred  = parent->vlog_deferred;
}

static bool elab_new_errors(const elab_ctx_t *ctx)
{
   return error_count() - ctx->errors;
}

static void elab_lower(tree_t b, elab_ctx_t *ctx)
{
   tree_t hier = tree_decl(b, 0);
   assert(tree_kind(hier) == T_HIER);

   if (tree_subkind(hier) == T_VERILOG)
      vlog_lower_block(ctx->mir, vlog_scope_alias(ctx->parent),
                       vlog_scope_alias(ctx), b);
   else {
      ctx->lowered = lower_instance(ctx->registry, ctx->parent->lowered,
                                    ctx->cover, ctx->cscope, b);

#ifdef DEBUG
      if (ctx->cloned != NULL) {
         // Cloned blocks must have identical layout
         mir_unit_t *new = mir_get_unit(ctx->mir, ctx->dotted);
         mir_unit_t *orig = mir_get_unit(ctx->mir, ctx->cloned);
         mir_compare_layout(new, orig);
      }
#endif
   }

   if (ctx->inst != NULL)
      diag_add_hint_fn(elab_hint_fn, ctx->inst);

   ctx->scope = create_scope(ctx->model, b, ctx->parent->scope);

   if (ctx->inst != NULL)
      diag_remove_hint_fn(elab_hint_fn, ctx->inst);
}

static bool elab_can_clone_instance(elab_instance_t *ei, const elab_ctx_t *ctx)
{
   if (ei == NULL)
      return false;
   else if (ei->cscope == NULL && ctx->cscope == NULL)
      return true;

   return cover_compatible_spec(ctx->cover, ei->cscope, ctx->cscope);
}

static void elab_verilog_ports(vlog_node_t inst, elab_instance_t *ei,
                               const elab_ctx_t *ctx)
{
   assert(vlog_kind(inst) == V_MOD_INST);

   const int nports = vlog_ports(ei->body);
   const int nparams = vlog_params(inst);

   if (nparams > nports) {
      error_at(vlog_loc(inst), "expected at most %d port connections for "
               "module %s but found %d", nports, istr(vlog_ident2(ei->body)),
               nparams);
      return;
   }

   LOCAL_BIT_MASK used;
   mask_init(&used, nparams);

   for (int i = 0; i < nports; i++) {
      vlog_node_t port = vlog_ref(vlog_port(ei->body, i)), conn = NULL;
      ident_t port_name = vlog_ident(port);

      if (i < nparams) {
         conn = vlog_param(inst, i);
         assert(vlog_kind(conn) == V_PORT_CONN);

         if (vlog_has_ident(conn) && vlog_ident(conn) != port_name)
            conn = NULL;
         else
            mask_set(&used, i);
      }

      if (conn == NULL) {
         for (int j = 0; j < nparams; j++) {
            vlog_node_t cj = vlog_param(inst, j);
            if (vlog_has_ident(cj) && vlog_ident(cj) == port_name) {
               conn = cj;
               mask_set(&used, j);
               break;
            }
         }
      }

      if (conn == NULL) {
         diag_t *d = diag_new(DIAG_WARN, vlog_loc(inst));
         diag_printf(d, "missing port connection for '%pi'", port_name);
         diag_hint(d, vlog_loc(port), "'%pi' declared here", port_name);
         diag_hint(d, vlog_loc(inst), "instance '%pi'", vlog_ident(inst));
         diag_emit(d);
         continue;
      }
      else if (!vlog_has_value(conn))
         continue;

      vlog_node_t value = vlog_value(conn);

      if (vlog_subkind(port) != V_PORT_INPUT && !vlog_is_net(value)) {
         diag_t *d = diag_new(DIAG_ERROR, vlog_loc(conn));
         if (vlog_kind(value) == V_REF)
            diag_printf(d, "'%s'", istr(vlog_ident(value)));
         else
            diag_printf(d, "expression");
         diag_printf(d, " cannot be driven by continuous assignment from "
                     "port '%s'", istr(vlog_ident(port)));
         diag_emit(d);
         continue;
      }

      vlog_node_t map = vlog_new(V_PORT_MAP);
      vlog_set_ref(map, port);
      vlog_set_value(map, value);
      vlog_set_loc(map, vlog_loc(conn));

      tree_t wrap = tree_new(T_VERILOG);
      tree_set_ident(wrap, vlog_ident(port));
      tree_set_vlog(wrap, map);
      tree_set_loc(wrap, vlog_loc(port));

      tree_add_decl(ctx->out, wrap);
   }

   if (mask_popcount(&used) == nparams)
      return;

   for (int i = 0; i < nparams; i++) {
      if (!mask_test(&used, i)) {
         vlog_node_t conn = vlog_param(inst, i);
         assert(vlog_has_ident(conn));

         error_at(vlog_loc(conn), "port '%pi' not found in %pi",
                  vlog_ident(conn), vlog_ident2(ei->body));
      }
   }
}

static ident_t elab_qualify_vlog_module(lib_t lib, ident_t modname)
{
   LOCAL_TEXT_BUF tb = tb_new();
   tb_istr(tb, lib_name(lib));
   tb_append(tb, '.');
   tb_istr(tb, modname);
   tb_upcase(tb);
   return ident_new(tb_get(tb));
}

////////////////////////////////////////////////////////////////////////////////
// Verilog hierarchical reference resolver.
//
// Runs at the end of each scope's elaboration.  Walks the scope's
// V_HIER_REF nodes and binds I_REF (final tail decl) + I_VALUE
// (target body) for use by lowering, %m runtime, VPI, VCD, and
// reheat.  All consumers of the user-visible canonical name go
// through `vlog_canonical_scope_name` (vlog-util.h).
//
// Discipline 3 (kind-agnostic scope walk): the lookup helpers below
// dispatch on the abstract notion of "is this node a scope-carrier
// with a list of decls" -- the per-kind dispatch is confined to the
// accessors `vlog_scope_decls_node` / `vlog_scope_children_walk`.
//
// Discipline 1 (generic decl lookup): the tail-identifier search
// enumerates every decl in the target scope without branching on
// decl kind.

typedef struct {
   vlog_node_t body;         // target V_INST_BODY / V_BLOCK
   ident_t     dotted;       // canonical dotted MIR unit name
   vlog_node_t parent_body;  // scope containing target instance
} hier_target_t;

// Returns the body+dotted for a child instance / named block
// labelled `label` within `parent_body`.  `parent_dotted` is the
// dotted MIR ident of `parent_body`; the child's dotted ident is
// derived from it.  If the child cannot be resolved, returns
// {NULL, NULL}.
static hier_target_t elab_find_vlog_child(vlog_node_t parent_body,
                                          ident_t parent_dotted,
                                          ident_t label,
                                          const elab_ctx_t *ctx)
{
   hier_target_t miss = { NULL, NULL };
   if (parent_body == NULL || label == NULL)
      return miss;

   const int nstmts = vlog_stmts(parent_body);
   for (int i = 0; i < nstmts; i++) {
      vlog_node_t s = vlog_stmt(parent_body, i);

      switch (vlog_kind(s)) {
      case V_INST_LIST:
         {
            const int nlist = vlog_stmts(s);
            for (int j = 0; j < nlist; j++) {
               vlog_node_t inst = vlog_stmt(s, j);
               if (vlog_kind(inst) != V_MOD_INST)
                  continue;
               if (vlog_ident(inst) != label)
                  continue;

               // Found the V_MOD_INST.  Look up its elaborated body
               // in modcache (key = module object, value = mod_cache_t
               // whose `instances` ghash maps V_INST_LIST -> ei).
               ident_t qual = elab_qualify_vlog_module(ctx->library,
                                                       vlog_ident(s));
               object_t *mod_obj =
                  lib_get_generic(ctx->library, qual, NULL);
               if (mod_obj == NULL)
                  return miss;

               mod_cache_t *mc = hash_get(ctx->modcache, mod_obj);
               if (mc == NULL || mc->instances == NULL)
                  return miss;

               elab_instance_t *ei = ghash_get(mc->instances, s);
               if (ei == NULL)
                  return miss;

               // Child's dotted MIR ident is the elab_instance's
               // block name -- elab_verilog_module set it to
               // ndotted = parent_dotted.label.  But for clones,
               // ei->block is the FIRST clone's block.  Compute
               // parent_dotted.label directly to handle every clone.
               hier_target_t out = {
                  .body        = ei->body,
                  .dotted      = ident_prefix(parent_dotted, label, '.'),
                  .parent_body = parent_body,
               };
               return out;
            }
         }
         break;

      case V_BLOCK:
         if (vlog_has_ident(s) && vlog_ident(s) == label) {
            hier_target_t out = {
               .body        = s,
               .dotted      = ident_prefix(parent_dotted, label, '.'),
               .parent_body = parent_body,
            };
            return out;
         }
         break;

      default:
         break;
      }
   }

   // Check blockcache for generate/named blocks registered during
   // elaboration.  This avoids mutating the (possibly shared) parent
   // body and works for blocks that don't appear in the original stmts.
   if (ctx->blockcache != NULL) {
      ident_t child_dotted = ident_prefix(parent_dotted, label, '.');
      vlog_node_t cached = hash_get(ctx->blockcache, child_dotted);
      if (cached != NULL) {
         hier_target_t out = {
            .body        = cached,
            .dotted      = child_dotted,
            .parent_body = parent_body,
         };
         return out;
      }
   }

   return miss;
}

// Generic decl-by-name search across any scope-carrying node
// (V_INST_BODY, V_BLOCK, V_FORK, V_PROGRAM, V_MODULE, future
// V_INTERFACE / V_BIND targets).  No per-kind branch on the decl --
// satisfies discipline 1.
static vlog_node_t elab_find_vlog_decl(vlog_node_t scope, ident_t name)
{
   if (scope == NULL || name == NULL)
      return NULL;

   const int ndecls = vlog_decls(scope);
   for (int i = 0; i < ndecls; i++) {
      vlog_node_t d = vlog_decl(scope, i);
      if (vlog_has_ident(d) && vlog_ident(d) == name)
         return d;
   }

   // Also search the scope's port list (relevant for V_INST_BODY).
   if (vlog_kind(scope) == V_INST_BODY || vlog_kind(scope) == V_MODULE
       || vlog_kind(scope) == V_PROGRAM || vlog_kind(scope) == V_PRIMITIVE) {
      const int nports = vlog_ports(scope);
      for (int i = 0; i < nports; i++) {
         vlog_node_t p = vlog_port(scope, i);
         if (vlog_has_ident(p) && vlog_ident(p) == name)
            return p;
      }
   }

   return NULL;
}

// Walks `prefix` segments separated by `.`, descending through
// scopes via elab_find_vlog_child.  Returns the target body + dotted
// name that corresponds to the final segment, or {NULL,NULL} on
// failure.
static hier_target_t elab_walk_vlog_path(vlog_node_t start,
                                         ident_t start_dotted,
                                         ident_t prefix,
                                         const elab_ctx_t *ctx)
{
   hier_target_t miss = { NULL, NULL };
   if (start == NULL)
      return miss;

   hier_target_t cur = { start, start_dotted };
   ident_t walk = prefix;
   ident_t seg;

   while ((seg = ident_walk_selected(&walk)) != NULL) {
      cur = elab_find_vlog_child(cur.body, cur.dotted, seg, ctx);
      if (cur.body == NULL)
         return miss;
   }

   return cur;
}

// Best-effort first pass: resolve single-segment downward hier-refs
// to child parameters so that constant expressions (reg widths,
// generate conditions) can be evaluated during vlog_lower_instance.
// Called before vlog_trans/vlog_lower_instance for the current body.
typedef struct {
   vlog_node_t body;
   const elab_ctx_t *ctx;
} pre_resolve_ctx_t;

static void elab_pre_resolve_cb(vlog_node_t v, void *context)
{
   pre_resolve_ctx_t *pctx = context;

   if (vlog_has_value(v))
      return;   // Already resolved

   ident_t prefix = vlog_has_ident2(v) ? vlog_ident2(v) : NULL;
   ident_t tail = vlog_ident(v);
   if (prefix == NULL || tail == NULL)
      return;

   // Only handle single-segment prefixes (e.g., `u.WIDTH`).
   ident_t walk = prefix;
   ident_t head = ident_walk_selected(&walk);
   if (walk != NULL)
      return;   // Multi-segment, skip

   // Find V_MOD_INST named `head` in the body's stmts.
   vlog_node_t body = pctx->body;
   const int nstmts = vlog_stmts(body);
   for (int i = 0; i < nstmts; i++) {
      vlog_node_t s = vlog_stmt(body, i);
      if (vlog_kind(s) != V_INST_LIST)
         continue;

      const int ninst = vlog_stmts(s);
      for (int j = 0; j < ninst; j++) {
         vlog_node_t inst = vlog_stmt(s, j);
         if (vlog_kind(inst) != V_MOD_INST || vlog_ident(inst) != head)
            continue;

         // Found. Load the module from library.
         ident_t qual = elab_qualify_vlog_module(pctx->ctx->library,
                                                 vlog_ident(s));
         object_t *obj = lib_get_generic(pctx->ctx->library, qual, NULL);
         if (obj == NULL)
            return;

         vlog_node_t mod = vlog_from_object(obj);
         if (mod == NULL)
            return;

         // Search the module's decls for a parameter matching `tail`.
         const int ndecls = vlog_decls(mod);
         for (int k = 0; k < ndecls; k++) {
            vlog_node_t d = vlog_decl(mod, k);
            if ((vlog_kind(d) == V_PARAM_DECL || vlog_kind(d) == V_LOCALPARAM)
                && vlog_has_ident(d) && vlog_ident(d) == tail) {
               // Bind the parameter: check for override from the inst.
               vlog_node_t value = vlog_has_value(d) ? vlog_value(d) : NULL;
               const int nparams = vlog_params(inst);
               for (int p = 0; p < nparams; p++) {
                  vlog_node_t pm = vlog_param(inst, p);
                  if (vlog_has_ident(pm) && vlog_ident(pm) == tail) {
                     value = vlog_value(pm);
                     break;
                  }
                  else if (!vlog_has_ident(pm) && p == k) {
                     // Positional override matching the k-th param
                     value = vlog_value(pm);
                     break;
                  }
               }

               if (value != NULL) {
                  // Set I_REF to the parameter decl so vlog_get_const
                  // can follow the chain to the value.
                  vlog_set_ref(v, d);
               }
               return;
            }
         }
         return;
      }
   }
}

static void elab_pre_resolve_hier_params(vlog_node_t body,
                                         const elab_ctx_t *ctx)
{
   pre_resolve_ctx_t pctx = { body, ctx };

   // Walk decl data types to find V_HIER_REFs in dimension expressions
   // (e.g., reg [u.WIDTH-1:0]).  Data types and generate conditions are
   // shared with the original module, so vlog_visit_only on the body
   // alone wouldn't reach them — we must traverse each range bound
   // explicitly.  Using vlog_visit_only on each bound expression handles
   // all expression kinds (V_BINARY, V_UNARY, V_COND_EXPR, V_CONCAT,
   // ...) without a fragile per-kind manual walk.
   const int ndecls = vlog_decls(body);
   for (int i = 0; i < ndecls; i++) {
      vlog_node_t d = vlog_decl(body, i);
      const vlog_kind_t dk = vlog_kind(d);
      if (dk != V_VAR_DECL && dk != V_NET_DECL && dk != V_PORT_DECL)
         continue;
      if (!vlog_has_type(d))
         continue;
      vlog_node_t dt = vlog_type(d);
      if (vlog_kind(dt) != V_DATA_TYPE)
         continue;
      const int nranges = vlog_ranges(dt);
      for (int r = 0; r < nranges; r++) {
         vlog_node_t range = vlog_range(dt, r);
         vlog_visit_only(vlog_left(range), elab_pre_resolve_cb,
                         &pctx, V_HIER_REF);
         vlog_visit_only(vlog_right(range), elab_pre_resolve_cb,
                         &pctx, V_HIER_REF);
      }
   }

   // Walk generate conditions and inner block decl types (both shared
   // with the original module — the visitor can't reach them from body).
   const int nstmts = vlog_stmts(body);
   for (int i = 0; i < nstmts; i++) {
      vlog_node_t s = vlog_stmt(body, i);
      if (vlog_kind(s) != V_IF_GENERATE)
         continue;
      const int nconds = vlog_conds(s);
      for (int j = 0; j < nconds; j++) {
         vlog_node_t c = vlog_cond(s, j);
         if (vlog_has_value(c))
            vlog_visit_only(vlog_value(c), elab_pre_resolve_cb,
                            &pctx, V_HIER_REF);
         // Walk decl types inside generate blocks.
         const int cstmts = vlog_stmts(c);
         for (int k = 0; k < cstmts; k++) {
            vlog_node_t cs = vlog_stmt(c, k);
            if (vlog_kind(cs) != V_BLOCK)
               continue;
            const int bndecls = vlog_decls(cs);
            for (int m = 0; m < bndecls; m++) {
               vlog_node_t bd = vlog_decl(cs, m);
               if (!vlog_has_type(bd))
                  continue;
               vlog_node_t bdt = vlog_type(bd);
               const int bnr = vlog_ranges(bdt);
               for (int n = 0; n < bnr; n++) {
                  vlog_node_t br = vlog_range(bdt, n);
                  vlog_visit_only(vlog_left(br), elab_pre_resolve_cb,
                                  &pctx, V_HIER_REF);
                  vlog_visit_only(vlog_right(br), elab_pre_resolve_cb,
                                  &pctx, V_HIER_REF);
               }
            }
         }
      }
   }

   // Walk remaining hier-refs reachable through the copied object
   // graph (e.g., in process statements).
   vlog_visit_only(body, elab_pre_resolve_cb, &pctx, V_HIER_REF);
}

// Build a per-clone alias chain by prefixing each segment of `path`
// onto `base` with dot separators.
// Resolves one V_HIER_REF node in `body`.  Sets I_REF (tail decl)
// and I_VALUE (target body) on success.  On failure leaves slots
// unset and emits a diagnostic with the full attempted path and the
// failing scope.
static void elab_resolve_one_hier_ref(vlog_node_t v, vlog_node_t body,
                                      const elab_ctx_t *ctx)
{
   if (vlog_has_value(v))
      return;   // Already resolved (e.g. by an earlier pass).

   ident_t prefix = vlog_ident2(v);
   ident_t tail = vlog_ident(v);
   const vlog_hier_anchor_t anchor = vlog_subkind(v);

   // Find the scope at which to BEGIN walking path segments.  For
   // RELATIVE refs this is the current body or a chain ancestor;
   // for ABS_ROOT it is the elaboration root.
   hier_target_t resolved = { NULL, NULL };
   const elab_ctx_t *found_ctx = NULL;

   if (anchor == V_HIER_ABS_UNIT) {
      diag_t *d = diag_new(DIAG_ERROR, vlog_loc(v));
      diag_printf(d, "$unit hierarchical references are not yet supported");
      diag_emit(d);
      return;
   }
   else if (anchor == V_HIER_ABS_ROOT) {
      // $root.<top>.<...>: the first segment names one of the
      // synthetic-root's direct children (any sibling top — the
      // user's design top, glbl, a second design, etc.).  Probe
      // scope_tree to find it by composing <library>.<root_head>;
      // then descend via scope_tree probes for any remaining
      // segments.  This is what makes $root.glbl.GSR reach a
      // sibling glbl top under the synthetic root.
      ident_t walk = prefix;
      ident_t root_head = ident_walk_selected(&walk);

      if (root_head != NULL && ctx->scope_tree != NULL) {
         // Walk up to the top-level scope under the synthetic root.
         // The chain built by elab_resolve_all_vlog_hier_refs only
         // contains real scopes (not the synth root), so r->parent
         // hitting NULL means r IS a top-level sibling.  Its dotted
         // is <library>.<short_label>; strip the short label to get
         // the library prefix that all sibling tops share.
         const elab_ctx_t *r = ctx;
         while (r->parent != NULL && !r->parent->is_synthetic_root)
            r = r->parent;
         ident_t root_prefix = (r->parent != NULL && r->parent->is_synthetic_root)
            ? r->parent->dotted
            : (r->dotted ? ident_runtil(r->dotted, '.') : NULL);

         if (root_prefix != NULL) {
            ident_t top_dotted = ident_prefix(root_prefix, root_head, '.');
            hier_node_t *tn = hash_get(ctx->scope_tree, top_dotted);
            if (tn != NULL) {
               // Descend any remaining segments via scope_tree.
               hier_node_t *tgt = tn;
               bool ok = true;
               ident_t w = walk;
               ident_t seg;
               while ((seg = ident_walk_selected(&w)) != NULL) {
                  ident_t nd = ident_prefix(tgt->ids.dotted, seg, '.');
                  hier_node_t *next = hash_get(ctx->scope_tree, nd);
                  if (next == NULL) { ok = false; break; }
                  tgt = next;
               }
               if (ok && tgt->vlog_body != NULL) {
                  resolved.body        = tgt->vlog_body;
                  resolved.dotted      = tgt->ids.dotted;
                  resolved.parent_body =
                     tgt->parent ? tgt->parent->vlog_body : NULL;
               }
            }
         }
      }
   }
   else {
      // RELATIVE: split prefix into head + rest.  Walk up the ctx
      // chain looking for a scope that contains `head` as a child.
      ident_t walk = prefix;
      ident_t head = ident_walk_selected(&walk);
      ident_t rest = walk;

      // Rooted-absolute check: if `head` matches the topmost body's
      // short label, treat the ref as absolute (vlog62 / vlog96).
      const elab_ctx_t *r = ctx;
      while (r->parent != NULL && r->parent->vlog_body != NULL)
         r = r->parent;
      if (r->vlog_body != NULL && head != NULL
          && r->dotted != NULL
          && head == ident_rfrom(r->dotted, '.')) {
         resolved = elab_walk_vlog_path(r->vlog_body, r->dotted,
                                        rest, ctx);
      }

      // Walk up the context chain searching for a scope containing
      // `head` as a child.  For each ancestor we take its OWN
      // vlog_body from scope_tree — ctx->vlog_body may be inherited
      // from a Verilog ancestor above an intervening VHDL scope,
      // which would make us search the wrong body and mint a
      // nonexistent dotted path.  VHDL ancestors are transparent
      // waypoints: their own body is NULL, so the loop skips them
      // and keeps walking up.  This is what lets a Verilog hier-ref
      // reach a Verilog sibling that sits above a VHDL wrapper
      // (the glbl pattern under a VHDL design top).
      for (const elab_ctx_t *cur = ctx;
           resolved.body == NULL && cur != NULL; cur = cur->parent) {

         if (cur->dotted == NULL || ctx->scope_tree == NULL)
            continue;

         hier_node_t *cur_node = hash_get(ctx->scope_tree, cur->dotted);
         if (cur_node == NULL)
            continue;

         // First: if this ancestor owns a Verilog body, try the
         // language-specific search (it knows about V_MOD_INST,
         // V_BLOCK, and the blockcache fallback in one place).
         if (cur_node->vlog_body != NULL) {
            hier_target_t child = elab_find_vlog_child(cur_node->vlog_body,
                                                       cur->dotted, head,
                                                       ctx);
            if (child.body != NULL) {
               found_ctx = cur;
               if (rest != NULL)
                  resolved = elab_walk_vlog_path(child.body, child.dotted,
                                                 rest, ctx);
               else
                  resolved = child;
               continue;
            }
         }

         // Second: language-neutral probe.  For VHDL ancestors this
         // is the only search; for Verilog ancestors it catches
         // scope kinds elab_find_vlog_child doesn't enumerate (the
         // scope_tree is populated at every elab_push_scope).
         ident_t candidate = ident_prefix(cur->dotted, head, '.');
         hier_node_t *head_node = hash_get(ctx->scope_tree, candidate);
         if (head_node == NULL)
            continue;

         found_ctx = cur;
         hier_node_t *tgt = head_node;
         bool ok = true;
         ident_t w = rest;
         ident_t seg;
         while ((seg = ident_walk_selected(&w)) != NULL) {
            ident_t nd = ident_prefix(tgt->ids.dotted, seg, '.');
            hier_node_t *next = hash_get(ctx->scope_tree, nd);
            if (next == NULL) {
               ok = false;
               break;
            }
            tgt = next;
         }
         if (ok && tgt->vlog_body != NULL) {
            resolved.body        = tgt->vlog_body;
            resolved.dotted      = tgt->ids.dotted;
            resolved.parent_body = tgt->parent ? tgt->parent->vlog_body : NULL;
         }
      }
   }

   if (resolved.body == NULL) {
      ident_t walk_copy = prefix;
      ident_t head = ident_walk_selected(&walk_copy);

      diag_t *d = diag_new(DIAG_ERROR, vlog_loc(v));
      if (found_ctx != NULL) {
         // Head resolved but a mid-segment failed — show the full
         // user-written path so the user can see which part is wrong.
         diag_printf(d, "no visible declaration for '%s.%s'",
                     istr(prefix), istr(tail));
      }
      else {
         diag_printf(d, "no visible declaration for '%s'", istr(head));
      }
      if (body != NULL && vlog_has_ident(body))
         diag_hint(d, vlog_loc(v), "in scope '%s'",
                   istr(vlog_canonical_scope_name(body)));
      diag_emit(d);
      return;
   }

   // Look up the tail decl in the resolved target body.
   vlog_node_t decl = elab_find_vlog_decl(resolved.body, tail);
   if (decl == NULL) {
      diag_t *d = diag_new(DIAG_ERROR, vlog_loc(v));
      diag_printf(d, "no visible declaration for '%s' in %s",
                  istr(tail),
                  istr(vlog_canonical_scope_name(resolved.body)));
      diag_emit(d);
      return;
   }

   // IEEE 1364 §12.4.3: a genvar is scoped to its generate block and
   // must not be accessible by hierarchical path.
   if (vlog_kind(decl) == V_GENVAR_DECL) {
      diag_t *d = diag_new(DIAG_ERROR, vlog_loc(v));
      diag_printf(d, "genvar '%s' is not accessible by hierarchical path",
                  istr(tail));
      diag_emit(d);
      return;
   }

   // Bind the resolver outputs.  I_VALUE gets the PARENT scope (the
   // body that contains the target instance) so lowering can compute
   // the per-instance alias as `vlog_ident(parent).last_seg`.  This
   // matches the alias used by instance_init in the block function.
   // I_REF gets the tail decl (overwriting the parse-time head decl).
   vlog_node_t parent_scope = resolved.parent_body ?: resolved.body;

   // When the parent scope is a V_BLOCK (e.g. generate block), look
   // up the elaborated V_INST_BODY from blockcache for the correct
   // canonical scope name.  The V_BLOCK itself has only the user-
   // visible label, not the fully-qualified MIR unit name.
   if (parent_scope != NULL && vlog_kind(parent_scope) == V_BLOCK
       && ctx->blockcache != NULL) {
      // resolved.dotted = parent.child (e.g. WORK.vlog61.gen_if.u).
      // The parent's dotted is resolved.dotted with last segment stripped.
      ident_t parent_dotted = ident_runtil(resolved.dotted, '.');
      if (parent_dotted != NULL) {
         vlog_node_t inst_body = hash_get(ctx->blockcache, parent_dotted);
         if (inst_body != NULL)
            parent_scope = inst_body;
      }
   }

   vlog_set_ref(v, decl);
   vlog_set_value(v, parent_scope);

   // I_IDENT2 is the resolved target scope's canonical dotted path.
   // vlog_lower_block registers each scope under its dotted path
   // (as an alias to the shared module template), and uses the
   // same dotted path as instance_init's PUTPRIV key.  So
   // link_package(dotted) at runtime resolves to the right JIT
   // handle and finds the per-scope privdata.  NULL signals "decl
   // is directly in the parent scope" — vlog_hier_unit_alias falls
   // back to vlog_canonical_scope_name in that case.
   if (prefix != NULL)
      vlog_set_ident2(v, resolved.dotted);
   else
      vlog_set_ident2(v, NULL);
}

typedef struct {
   vlog_node_t       body;
   const elab_ctx_t *ctx;
} hier_resolve_ctx_t;

static void elab_resolve_visit_cb(vlog_node_t v, void *context)
{
   hier_resolve_ctx_t *rc = context;
   elab_resolve_one_hier_ref(v, rc->body, rc->ctx);
}

static void elab_propagate_tcall_ref(vlog_node_t v, void *context)
{
   // For V_USER_TCALL / V_USER_FCALL with a V_HIER_REF in I_VALUE:
   // the inner V_HIER_REF was already resolved; propagate its I_REF
   // to the enclosing call node so lowering can find the target.
   if (!vlog_has_value(v))
      return;
   vlog_node_t href = vlog_value(v);
   if (vlog_kind(href) != V_HIER_REF || !vlog_has_ref(href))
      return;
   vlog_set_ref(v, vlog_ref(href));
}

static void elab_check_disable_target(vlog_node_t v, void *context)
{
   // IEEE 1364 §9.6.2: disable target must name a block or task.
   if (!vlog_has_value(v))
      return;
   vlog_node_t href = vlog_value(v);
   if (vlog_kind(href) != V_HIER_REF || !vlog_has_ref(href))
      return;

   vlog_node_t decl = vlog_ref(href);
   switch (vlog_kind(decl)) {
   case V_BLOCK:
   case V_TASK_DECL:
      break;   // Valid disable targets.
   default:
      {
         diag_t *d = diag_new(DIAG_ERROR, vlog_loc(v));
         diag_printf(d, "target of disable is not a named block or task");
         diag_emit(d);
      }
   }
}

static void elab_resolve_vlog_hier_refs(vlog_node_t body,
                                        const elab_ctx_t *ctx)
{
   if (body == NULL)
      return;

   hier_resolve_ctx_t rc = { .body = body, .ctx = ctx };

   // Iterate own stmts (initial / always / continuous-assign hold
   // the V_HIER_REF leaves).  Decls may contain hier-refs in
   // initialisers (e.g. `reg [u.WIDTH-1:0] x;` when WIDTH is
   // resolvable at elab time).  vlog_visit_only handles the
   // recursive descent for us, only invoking the callback on
   // V_HIER_REF nodes.
   const int nstmts = vlog_stmts(body);
   for (int i = 0; i < nstmts; i++)
      vlog_visit_only(vlog_stmt(body, i), elab_resolve_visit_cb, &rc,
                      V_HIER_REF);

   const int ndecls = vlog_decls(body);
   for (int i = 0; i < ndecls; i++)
      vlog_visit_only(vlog_decl(body, i), elab_resolve_visit_cb, &rc,
                      V_HIER_REF);

   // Propagate resolved I_REF to V_USER_TCALL / V_USER_FCALL nodes
   // that carry a hier-ref on I_VALUE.
   for (int i = 0; i < nstmts; i++) {
      vlog_visit_only(vlog_stmt(body, i), elab_propagate_tcall_ref,
                      NULL, V_USER_TCALL);
      vlog_visit_only(vlog_stmt(body, i), elab_propagate_tcall_ref,
                      NULL, V_USER_FCALL);
   }

   // Check V_DISABLE targets after resolution.
   for (int i = 0; i < nstmts; i++)
      vlog_visit_only(vlog_stmt(body, i), elab_check_disable_target,
                      NULL, V_DISABLE);
}

static void elab_verilog_module(tree_t comp, ident_t label, vlog_node_t mod,
                                vlog_node_t list, vlog_node_t inst,
                                const elab_ctx_t *ctx)
{
   ident_t ndotted = ident_prefix(ctx->dotted, label, '.');

   elab_ctx_t new_ctx = {
      .dotted = ndotted,
   };
   elab_inherit_context(&new_ctx, ctx);

   tree_t b = tree_new(T_BLOCK);
   tree_set_ident(b, label);
   tree_set_loc(b, tree_loc(ctx->out));

   tree_add_stmt(ctx->out, b);
   new_ctx.out = b;

   mod_cache_t *mc = elab_cached_module(vlog_to_object(mod), &new_ctx);
   mc->count++;

   elab_instance_t *ei = ghash_get(mc->instances, list);
   if (!elab_can_clone_instance(ei, &new_ctx)) {
      ident_t id = ident_sprintf("%s#%d", istr(vlog_ident(mod)), mc->unique);

      ei = pool_calloc(ctx->pool, sizeof(elab_instance_t));
      ei->body = vlog_new_instance(mod, list, id);

      ei->wrap = tree_new(T_VERILOG);
      tree_set_loc(ei->wrap, vlog_loc(mod));
      tree_set_ident(ei->wrap, vlog_ident(mod));
      tree_set_vlog(ei->wrap, ei->body);

      ei->block = tree_new(T_BLOCK);
      tree_set_loc(ei->block, vlog_loc(list));
      tree_set_ident(ei->block, ndotted);

      // Best-effort first pass: resolve downward hier-refs to child
      // parameters so constant expressions in reg widths and generate
      // conditions can be evaluated during lowering.
      elab_pre_resolve_hier_params(ei->body, &new_ctx);

      vlog_trans(ei->body, ei->block);
      vlog_lower_instance(ctx->mir, ei->body, NULL, ei->block);

      ghash_put(mc->instances, list, ei);
      mc->unique++;
   }

   new_ctx.cloned = vlog_ident(ei->body);

   // Compute the per-instance alias for the context chain.  The MIR
   // alias is registered inside vlog_lower_block (called by elab_lower
   // below); we compute the canonical name here via vlog_scope_alias
   // so both sides see the exact same ident.
   if (ctx->vlog_body != NULL) {
      ident_t alias = ident_prefix(vlog_scope_alias(ctx), label, '.');
      ident_t target = vlog_ident(ei->body);
      if (alias != target)
         new_ctx.inst_alias = alias;
   }

   // Set own vlog_body before push_scope so the scope-tree node
   // captures *this* module's body, not the parent's inherited one.
   new_ctx.vlog_body = ei->body;

   elab_push_scope(ei->wrap, &new_ctx);

   if (comp != NULL) {
      tree_t bind = elab_mixed_binding(comp, ei);
      if (bind != NULL)
         elab_ports(ei->block, bind, &new_ctx);
   }
   else
      elab_verilog_ports(inst, ei, &new_ctx);

   if (elab_new_errors(&new_ctx) == 0) {
      elab_decls(ei->block, &new_ctx);
      elab_verilog_processes(ei->body, &new_ctx);
   }

   if (elab_new_errors(&new_ctx) == 0)
      elab_lower(b, &new_ctx);

   if (elab_new_errors(&new_ctx) == 0)
      elab_verilog_sub_blocks(ei->body, &new_ctx);

   // Register this body for post-elaboration hier-ref resolution.
   if (new_ctx.vlog_deferred != NULL) {
      vlog_deferred_body_t db = {
         .body          = ei->body,
         .dotted        = new_ctx.dotted,
         .parent_body   = ctx->vlog_body,
         .parent_dotted = ctx->dotted,
         .inst_alias    = new_ctx.inst_alias,
      };
      APUSH(*new_ctx.vlog_deferred, db);
   }

   elab_pop_scope(&new_ctx);
}

static void elab_verilog_instance_list(vlog_node_t v, const elab_ctx_t *ctx)
{
   ident_t modname = vlog_ident(v);
   ident_t libname = lib_name(ctx->library);

   LOCAL_TEXT_BUF tb = tb_new();
   tb_istr(tb, libname);
   tb_append(tb, '.');
   tb_istr(tb, modname);
   tb_upcase(tb);

   ident_t qual = ident_new(tb_get(tb));

   object_t *obj = lib_get_generic(ctx->library, qual, NULL);
   if (obj == NULL) {
      error_at(vlog_loc(v), "module %s not found in library %s",
               istr(modname), istr(libname));
      return;
   }

   vlog_node_t mod = vlog_from_object(obj);
   if (mod == NULL) {
      error_at(&obj->loc, "unit %s is not a Verilog module", istr(qual));
      return;
   }
   else if (vlog_ident2(mod) != modname) {
      diag_t *d = diag_new(DIAG_ERROR, vlog_loc(v));
      diag_printf(d, "name of Verilog module %s in library unit %s "
                  "does not match name %s in module instance %s",
                  istr(vlog_ident2(mod)), istr(qual), istr(modname),
                  istr(vlog_ident(vlog_stmt(v, 0))));
      diag_hint(d, NULL, "this tool does not preserve case sensitivity "
                "in module names");
      diag_emit(d);
      return;
   }

   const int nstmts = vlog_stmts(v);
   for (int i = 0; i < nstmts; i++) {
      vlog_node_t inst = vlog_stmt(v, i);
      assert(vlog_kind(inst) == V_MOD_INST);

      elab_verilog_module(NULL, vlog_ident(inst), mod, v, inst, ctx);
   }
}

static void elab_verilog_block(vlog_node_t v, const elab_ctx_t *ctx)
{
   ident_t id = vlog_ident(v);

   tree_t b = tree_new(T_BLOCK);
   tree_set_ident(b, id);
   tree_set_loc(b, vlog_loc(v));

   tree_add_stmt(ctx->out, b);

   ident_t ndotted = ident_prefix(ctx->dotted, id, '.');

   elab_ctx_t new_ctx = {
      .out    = b,
      .dotted = ndotted,
   };
   elab_inherit_context(&new_ctx, ctx);

   // TODO: use elab_instance_t to cache this

   ident_t id2 = ident_prefix(ctx->dotted, vlog_ident(v), '%');
   vlog_node_t body = vlog_new_instance(v, NULL, id2);

   tree_t wrap = tree_new(T_VERILOG);
   tree_set_ident(wrap, id);
   tree_set_loc(wrap, vlog_loc(body));
   tree_set_vlog(wrap, body);

   tree_t block = tree_new(T_BLOCK);
   tree_set_loc(block, vlog_loc(v));
   tree_set_ident(block, ndotted);

   // Compute the per-instance alias before lowering so
   // vlog_lower_instance can register it (and reheat gets it for free).
   ident_t block_alias = NULL;
   if (ctx->vlog_body != NULL) {
      block_alias = ident_prefix(vlog_scope_alias(ctx), id, '.');
      ident_t target = vlog_ident(body);
      if (block_alias != target)
         new_ctx.inst_alias = block_alias;
      else
         block_alias = NULL;
   }

   vlog_trans(body, block);
   vlog_lower_instance(ctx->mir, body, ctx->cloned, block);

   new_ctx.cloned = vlog_ident(body);
   new_ctx.vlog_body = body;

   elab_push_scope(wrap, &new_ctx);

   elab_decls(block, &new_ctx);
   elab_verilog_processes(body, &new_ctx);

   elab_lower(b, &new_ctx);

   if (elab_new_errors(&new_ctx) == 0)
      elab_verilog_sub_blocks(v, &new_ctx);

   // Register the V_INST_BODY in blockcache keyed by its dotted path
   // so the resolver can find generate/named blocks without mutating
   // the (possibly shared) parent body's stmts list.
   if (new_ctx.blockcache != NULL)
      hash_put(new_ctx.blockcache, ndotted, body);

   if (new_ctx.vlog_deferred != NULL) {
      vlog_deferred_body_t db = {
         .body          = body,
         .dotted        = ndotted,
         .parent_body   = ctx->vlog_body,
         .parent_dotted = ctx->dotted,
         .inst_alias    = new_ctx.inst_alias,
      };
      APUSH(*new_ctx.vlog_deferred, db);
   }

   elab_pop_scope(&new_ctx);
}

static void *elab_verilog_for_generate_test_cb(jit_scalar_t *args, void *ctx)
{
   const uint64_t abits = args[0].integer;
   const uint64_t bbits = args[1].integer;
   return (void *)(uintptr_t)(abits == 1 && bbits == 0);
}

static void *elab_verilog_for_generate_index_cb(jit_scalar_t *args, void *ctx)
{
   const uint64_t abits = args[0].integer;
   const uint64_t bbits = args[1].integer;

   (void)bbits;   // TODO: check not X/Z

   int32_t *index = ctx;
   *index = abits;

   return NULL;
}

static void elab_verilog_if_generate(vlog_node_t v, const elab_ctx_t *ctx)
{
   void *context = *mptr_get(ctx->scope->privdata);
   const int nconds = vlog_conds(v);

   for (int i = 0; i < nconds; i++) {
      vlog_node_t c = vlog_cond(v, i);

      if (vlog_has_value(c)) {
         mir_unit_t *test = vlog_lower_thunk(ctx->mir, ctx->cloned,
                                             vlog_value(c));
         bool taken = jit_call_thunk2(ctx->jit, test, context,
                                      elab_verilog_for_generate_test_cb,
                                      NULL) != NULL;
         mir_unit_free(test);

         if (!taken)
            continue;
      }

      // First true condition (or else branch): elaborate its contents
      elab_verilog_sub_blocks(c, ctx);
      return;
   }
}

static void elab_verilog_for_generate(vlog_node_t v, const elab_ctx_t *ctx)
{
   mir_unit_t *init = vlog_lower_thunk(ctx->mir, ctx->cloned, vlog_left(v));
   mir_unit_t *test = vlog_lower_thunk(ctx->mir, ctx->cloned, vlog_value(v));
   mir_unit_t *step = vlog_lower_thunk(ctx->mir, ctx->cloned, vlog_right(v));

   void *context = *mptr_get(ctx->scope->privdata);

   jit_call_thunk2(ctx->jit, init, context, NULL, NULL);

   vlog_node_t genvar = vlog_ref(vlog_target(vlog_stmt(vlog_left(v), 0)));
   assert(vlog_kind(genvar) == V_GENVAR_DECL);

   vlog_node_t ref = vlog_new(V_REF);
   vlog_set_ref(ref, genvar);
   vlog_set_loc(ref, vlog_loc(genvar));

   mir_unit_t *get_var = vlog_lower_thunk(ctx->mir, ctx->cloned, ref);

   vlog_node_t s0 = vlog_stmt(v, 0);
   assert(vlog_kind(s0) == V_BLOCK);

   while (jit_call_thunk2(ctx->jit, test, context,
                          elab_verilog_for_generate_test_cb, NULL)) {
      int32_t index;
      jit_call_thunk2(ctx->jit, get_var, context,
                      elab_verilog_for_generate_index_cb, &index);

      // TODO: cache this in elab_instance_t
      vlog_node_t copy = vlog_generate_instance(s0, genvar, index, ctx->dotted);

      elab_verilog_block(copy, ctx);

      jit_call_thunk2(ctx->jit, step, context, NULL, NULL);
   }

   mir_unit_free(get_var);
   mir_unit_free(init);
   mir_unit_free(test);
   mir_unit_free(step);
}

static void elab_verilog_processes(vlog_node_t v, const elab_ctx_t *ctx)
{
   const int nstmts = vlog_stmts(v);
   for (int i = 0; i < nstmts; i++) {
      vlog_node_t s = vlog_stmt(v, i);

      switch (vlog_kind(s)) {
      case V_INITIAL:
      case V_ALWAYS:
      case V_ASSIGN:
      case V_UDP_TABLE:
      case V_GATE_INST:
         {
            tree_t w = tree_new(T_VERILOG);
            tree_set_ident(w, vlog_ident(s));
            tree_set_loc(w, vlog_loc(s));
            tree_set_vlog(w, s);

            tree_add_stmt(ctx->out, w);
         }
         break;
      case V_INST_LIST:
      case V_BLOCK:
      case V_IF_GENERATE:
      case V_FOR_GENERATE:
      case V_SPECIFY:
         break;
      default:
         fatal_at(vlog_loc(s), "sorry, this Verilog statement is not "
                  "currently supported");
      }
   }
}

static void elab_verilog_sub_blocks(vlog_node_t v, const elab_ctx_t *ctx)
{
   const int nstmts = vlog_stmts(v);
   for (int i = 0; i < nstmts; i++) {
      vlog_node_t s = vlog_stmt(v, i);

      const vlog_kind_t kind = vlog_kind(s);
      switch (kind) {
      case V_INST_LIST:
         elab_verilog_instance_list(s, ctx);
         break;
      case V_BLOCK:
         elab_verilog_block(s, ctx);
         break;
      case V_IF_GENERATE:
         elab_verilog_if_generate(s, ctx);
         break;
      case V_FOR_GENERATE:
         elab_verilog_for_generate(s, ctx);
         break;
      case V_SPECIFY:
         INIT_ONCE(warn_at(vlog_loc(s), "specify blocks are not currently "
                           "supported and will be ignored"));
         break;
      default:
         break;
      }
   }
}

static vlog_node_t elab_mixed_generics(tree_t comp, vlog_node_t mod,
                                       const elab_ctx_t *ctx)
{
   vlog_node_t stmt = vlog_new(V_MOD_INST);
   vlog_set_ident(stmt, vlog_ident2(mod));
   vlog_set_loc(stmt, tree_loc(comp));

   vlog_node_t list = vlog_new(V_INST_LIST);
   vlog_set_ident(list, vlog_ident2(mod));
   vlog_add_stmt(list, stmt);
   vlog_set_loc(list, tree_loc(comp));

   const int vhdl_ngenerics = tree_generics(comp);
   const int vlog_ndecls = vlog_decls(mod);

   LOCAL_BIT_MASK have;
   mask_init(&have, vhdl_ngenerics);

   bool named = false;
   for (int i = 0; i < vlog_ndecls; i++) {
      vlog_node_t d = vlog_decl(mod, i);
      if (vlog_kind(d) != V_PARAM_DECL)
         continue;

      ident_t name = vlog_ident(d);

      tree_t cg = NULL;
      int cpos = -1;
      for (int j = 0; j < vhdl_ngenerics; j++) {
         tree_t gj = tree_generic(comp, j);
         if (ident_casecmp(tree_ident(gj), name)) {
            cg = gj;
            mask_set(&have, (cpos = j));
            break;
         }
      }

      if (cg == NULL && vlog_has_value(d)) {
         named = true;
         continue;
      }
      else if (cg == NULL) {
         error_at(tree_loc(comp), "missing matching VHDL generic declaration "
                  "for Verilog parameter '%s' with no default value in "
                  "component %s", istr(name), istr(tree_ident(comp)));
         return NULL;
      }

      type_t ctype = tree_type(cg);
      type_t etype = std_type(NULL, STD_INTEGER);

      if (!type_eq(ctype, etype)) {
         error_at(tree_loc(cg), "generic %s should have type %s to match "
                  "corresponding Verilog parameter", istr(tree_ident(cg)),
                  type_pp(etype));
         return NULL;
      }

      tree_t value = find_generic_map(ctx->out, cpos, cg);
      if (value == NULL)
         fatal_at(tree_loc(ctx->out), "cannot get value of generic %s",
                  istr(tree_ident(cg)));

      number_t n = number_from_int(assume_int(value));

      vlog_node_t num = vlog_new(V_NUMBER);
      vlog_set_number(num, n);

      vlog_node_t pa = vlog_new(V_PARAM_ASSIGN);
      vlog_set_value(pa, num);
      if (named) vlog_set_ident(pa, vlog_ident(d));

      vlog_add_param(list, pa);
   }

   for (int i = 0; i < vhdl_ngenerics; i++) {
      if (!mask_test(&have, i)) {
         tree_t g = tree_generic(comp, i);
         diag_t *d = diag_new(DIAG_ERROR, tree_loc(g));
         diag_printf(d, "generic %s not found in Verilog module %s",
                     istr(tree_ident(g)), istr(vlog_ident2(mod)));
         diag_emit(d);
      }
   }

   return list;
}

static void elab_mixed_instance(tree_t inst, tree_t comp, vlog_node_t mod,
                                const elab_ctx_t *ctx)
{
   ident_t ndotted = ident_prefix(ctx->dotted, tree_ident(inst), '.');

   elab_ctx_t new_ctx = {
      .dotted = ndotted,
      .inst   = inst,
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

   if (elab_new_errors(&new_ctx) == 0)
      elab_lower(b, &new_ctx);

   vlog_node_t list = elab_mixed_generics(comp, mod, &new_ctx);
   if (list != NULL)
      elab_verilog_module(comp, vlog_ident2(mod), mod, list, NULL, &new_ctx);

   elab_pop_scope(&new_ctx);
}

static void elab_bind_components(tree_t block, tree_t config)
{
   const int nstmts = tree_stmts(block);
   for (int i = 0; i < nstmts; i++) {
      tree_t s = tree_stmt(block, i);
      switch (tree_kind(s)) {
      case T_INSTANCE:
         {
            if (tree_class(s) != C_COMPONENT || tree_has_spec(s))
               break;

            tree_t spec = NULL;
            const int ndecls = tree_decls(config);
            for (int j = 0; j < ndecls; j++) {
               tree_t d = tree_decl(config, j);
               if (tree_kind(d) != T_SPEC)
                  continue;
               else if (tree_ident2(d) != tree_ident2(s))
                  continue;

               bool apply = false;
               if (tree_has_ident(d)) {
                  ident_t match = tree_ident(d);
                  if (match == tree_ident(s) || match == well_known(W_ALL))
                     apply = true;
               }
               else if (spec == NULL)
                  apply = true;

               if (apply) spec = d;
            }

            tree_set_spec(s, spec);
         }
         break;

      case T_FOR_GENERATE:
      case T_IF_GENERATE:
      case T_BLOCK:
         {
            const int ndecls = tree_decls(config);
            for (int j = 0; j < ndecls; j++) {
               tree_t d = tree_decl(config, j);
               if (tree_kind(d) != T_BLOCK_CONFIG)
                  continue;
               else if (tree_ident(d) == tree_ident(s)) {
                  if (tree_kind(s) == T_IF_GENERATE)
                     elab_bind_components(tree_cond(s, 0), d);
                  else
                     elab_bind_components(s, d);
                  break;
               }
            }
         }
         break;

      default:
         break;
      }
   }
}

static void elab_cover_block(elab_ctx_t *ctx, tree_t unit)
{
   if (ctx->cover == NULL)
      return;

   tree_t block = ctx->out;
   cover_scope_t *parent = ctx->cscope;

   if (ctx->parent != NULL && tree_decls(ctx->parent->out) > 0) {
      tree_t hier = tree_decl(ctx->parent->out, 0);
      assert(tree_kind(hier) == T_HIER);

      if (tree_kind(tree_ref(hier)) == T_COMPONENT) {
         // Collapse this coverage scope with the block for the
         // component above
         block = ctx->parent->out;
         parent = ctx->parent->cscope;
      }
   }

   ctx->cscope = cover_create_block(ctx->cover, ctx->dotted, parent,
                                    block, unit);
}

static void elab_architecture(tree_t inst, tree_t arch, const elab_ctx_t *ctx)
{
   ident_t label = tree_ident(inst);
   ident_t ndotted = ident_prefix(ctx->dotted, label, '.');

   elab_ctx_t new_ctx = {
      .dotted = ndotted,
      .inst   = elab_parent_is_root(ctx->parent) ? NULL : inst,
   };
   elab_inherit_context(&new_ctx, ctx);

   tree_t b = tree_new(T_BLOCK);
   tree_set_ident(b, label);
   tree_set_loc(b, tree_loc(inst));

   tree_add_stmt(ctx->out, b);
   new_ctx.out = b;

   new_ctx.library = lib_require(ident_until(tree_ident(arch), '.'));

   elab_cover_block(&new_ctx, arch);

   mod_cache_t *mc = elab_cached_module(tree_to_object(arch), &new_ctx);
   mc->count++;

   elab_instance_t *ei = ghash_get(mc->instances, inst);
   if (elab_can_clone_instance(ei, &new_ctx))
      new_ctx.cloned = tree_ident(ei->block);
   else {
      ei = pool_calloc(ctx->pool, sizeof(elab_instance_t));
      ei->block = vhdl_architecture_instance(arch, inst, new_ctx.dotted);
      ei->cscope = new_ctx.cscope;

      elab_fold_generics(ei->block, &new_ctx);

      elab_context(arch);
      elab_context(tree_primary(arch));

      ghash_put(mc->instances, inst, ei);
      mc->unique++;
   }

   elab_push_scope(arch, &new_ctx);
   elab_generics(ei->block, inst, &new_ctx);
   elab_ports(ei->block, inst, &new_ctx);
   elab_decls(ei->block, &new_ctx);
   elab_vhdl_processes(ei->block, &new_ctx);

   if (error_count() == 0) {
      vhdl_cover_block(b, new_ctx.cover, new_ctx.cscope);
      elab_lower(b, &new_ctx);
      elab_vhdl_sub_blocks(ei->block, &new_ctx);
   }

   elab_pop_scope(&new_ctx);
}

static void elab_configuration(tree_t inst, tree_t unit, const elab_ctx_t *ctx)
{
   ident_t label = tree_ident(inst);
   ident_t ndotted = ident_prefix(ctx->dotted, label, '.');

   elab_ctx_t new_ctx = {
      .dotted = ndotted,
      .inst   = elab_parent_is_root(ctx->parent) ? NULL : inst,
   };
   elab_inherit_context(&new_ctx, ctx);

   tree_t b = tree_new(T_BLOCK);
   tree_set_ident(b, label);
   tree_set_loc(b, tree_loc(inst));

   tree_add_stmt(ctx->out, b);
   new_ctx.out = b;

   new_ctx.library = lib_require(ident_until(tree_ident(unit), '.'));

   tree_t config = tree_decl(unit, 0);
   assert(tree_kind(config) == T_BLOCK_CONFIG);

   tree_t arch = tree_ref(config);
   assert(tree_kind(arch) == T_ARCH);

   elab_cover_block(&new_ctx, arch);

   mod_cache_t *mc = elab_cached_module(tree_to_object(unit), &new_ctx);
   mc->count++;

   elab_instance_t *ei = ghash_get(mc->instances, inst);
   if (elab_can_clone_instance(ei, &new_ctx))
      new_ctx.cloned = tree_ident(ei->block);
   else {
      ei = pool_calloc(ctx->pool, sizeof(elab_instance_t));
      ei->block = vhdl_config_instance(config, inst, new_ctx.dotted);
      ei->cscope = new_ctx.cscope;

      elab_bind_components(ei->block, ei->block);
      elab_fold_generics(ei->block, &new_ctx);

      elab_context(arch);
      elab_context(tree_primary(arch));

      ghash_put(mc->instances, inst, ei);
      mc->unique++;
   }

   elab_push_scope(arch, &new_ctx);
   elab_generics(ei->block, inst, &new_ctx);
   elab_ports(ei->block, inst, &new_ctx);
   elab_decls(ei->block, &new_ctx);
   elab_vhdl_processes(ei->block, &new_ctx);

   if (error_count() == 0) {
      vhdl_cover_block(b, new_ctx.cover, new_ctx.cscope);
      elab_lower(b, &new_ctx);
      elab_vhdl_sub_blocks(ei->block, &new_ctx);
   }

   elab_pop_scope(&new_ctx);
}

static void elab_component(tree_t inst, tree_t comp, const elab_ctx_t *ctx)
{
   if (!tree_has_spec(inst)) {
      tree_t bind = elab_default_binding(inst, ctx);
      if (bind == NULL) {
         ident_t prefix = lib_name(ctx->library);
         ident_t qual = ident_prefix(prefix, tree_ident2(inst), '.');

         object_t *obj = lib_get_generic(ctx->library, qual, NULL);

         vlog_node_t mod = vlog_from_object(obj);
         if (mod != NULL) {
            elab_mixed_instance(inst, comp, mod, ctx);
            return;
         }
      }

      tree_t spec = tree_new(T_SPEC);
      tree_set_loc(spec, tree_loc(inst));
      tree_set_ident(spec, tree_ident(inst));
      tree_set_value(spec, bind);

      tree_set_spec(inst, spec);
   }

   ident_t ndotted = ident_prefix(ctx->dotted, tree_ident(inst), '.');

   elab_ctx_t new_ctx = {
      .dotted = ndotted,
      .inst   = inst,
   };
   elab_inherit_context(&new_ctx, ctx);

   mod_cache_t *mc = elab_cached_module(tree_to_object(comp), &new_ctx);
   mc->count++;

   elab_instance_t *ei = ghash_get(mc->instances, inst);
   if (elab_can_clone_instance(ei, &new_ctx))
      new_ctx.cloned = tree_ident(ei->block);
   else {
      ei = pool_calloc(ctx->pool, sizeof(elab_instance_t));
      ei->block = vhdl_component_instance(comp, inst, ndotted);
      ei->cscope = new_ctx.cscope;

      elab_fold_generics(ei->block, ctx);

      ghash_put(mc->instances, inst, ei);
      mc->unique++;

      // XXX: workaround for potentially different layouts
      if (!tree_frozen(comp))
         tree_set_global_flags(ctx->out, TREE_GF_INSTANCE_NAME
                               | TREE_GF_PATH_NAME);
   }

   tree_t b = tree_new(T_BLOCK);
   tree_set_ident(b, tree_ident(inst));
   tree_set_loc(b, tree_loc(inst));

   tree_add_stmt(ctx->out, b);
   new_ctx.out = b;

   elab_push_scope(comp, &new_ctx);
   elab_generics(ei->block, inst, &new_ctx);
   elab_ports(ei->block, inst, &new_ctx);

   if (elab_new_errors(&new_ctx) == 0) {
      elab_lower(b, &new_ctx);
      elab_vhdl_sub_blocks(ei->block, &new_ctx);
   }

   elab_pop_scope(&new_ctx);
}

static void elab_instance(tree_t t, const elab_ctx_t *ctx)
{
   if (ctx->depth == MAX_DEPTH) {
      diag_t *d = diag_new(DIAG_ERROR, tree_loc(t));
      diag_printf(d, "maximum instantiation depth of %d reached", MAX_DEPTH);
      diag_hint(d, NULL, "this is likely caused by unbounded recursion");
      diag_emit(d);
      return;
   }

   tree_t new = elab_override_instance_generics(t, ctx);

   tree_t ref = tree_ref(t);
   switch (tree_kind(ref)) {
   case T_ENTITY:
      {
         tree_t arch = elab_pick_arch(ref, tree_loc(new), ctx);
         elab_architecture(new, arch, ctx);
      }
      break;
   case T_ARCH:
      elab_architecture(new, ref, ctx);
      break;
   case T_COMPONENT:
      elab_component(new, ref, ctx);
      break;
   case T_CONFIGURATION:
      elab_configuration(new, ref, ctx);
      break;
   default:
      should_not_reach_here();
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
      case T_PSL_DECL:
      case T_ATTR_SPEC:
      case T_VERILOG:
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

   tree_set_ident(h, ctx->dotted);
   tree_set_ident2(h, ctx->cloned ?: ctx->dotted);

   tree_add_decl(ctx->out, h);

   // Register this scope in the language-neutral scope tree.  Phase 3
   // uses the tree for cross-language resolver walks; Phase 2 keeps
   // the registration so the infrastructure is ready.
   if (ctx->scope_tree != NULL && ctx->dotted != NULL) {
      hier_node_t *node = pool_calloc(ctx->pool, sizeof(hier_node_t));
      node->ids.inst_alias = ctx->inst_alias;
      node->ids.cloned     = ctx->cloned;
      node->ids.dotted     = ctx->dotted;
      node->label          = ident_rfrom(ctx->dotted, '.');
      node->tree_body      = ctx->out;
      node->lang           = (tree_kind(t) == T_VERILOG)
         ? HIER_LANG_VLOG : HIER_LANG_VHDL;
      node->vlog_body      = (node->lang == HIER_LANG_VLOG)
         ? ctx->vlog_body : NULL;

      ident_t parent_dotted = ident_runtil(ctx->dotted, '.');
      if (parent_dotted != NULL)
         node->parent = hash_get(ctx->scope_tree, parent_dotted);

      hash_put(ctx->scope_tree, ctx->dotted, node);
   }
}

static void elab_pop_scope(elab_ctx_t *ctx)
{
   if (ctx->lowered != NULL)
      unit_registry_finalise(ctx->registry, ctx->lowered);
}

static inline tree_t elab_eval_expr(tree_t t, const elab_ctx_t *ctx)
{
   void *context = *mptr_get(ctx->scope->privdata);
   return eval_must_fold(ctx->jit, t, ctx->registry, ctx->lowered, context);
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

      tree_t tlow = elab_eval_expr(tmp, ctx);
      if (folded_int(tlow, low)) {
         tree_set_subkind(tmp, ATTR_HIGH);

         tree_t thigh = elab_eval_expr(tmp, ctx);
         if (folded_int(thigh, high))
            return;
      }

      error_at(tree_loc(r), "generate range is not static");
      *low = *high = 0;
   }
   else if (!folded_bounds(r, low, high)) {
      tree_t left  = elab_eval_expr(tree_left(r), ctx);
      tree_t right = elab_eval_expr(tree_right(r), ctx);

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

   // XXX: workaround for potentially different layouts
   if (low != high)
      tree_set_global_flags(ctx->out, TREE_GF_INSTANCE_NAME
                            | TREE_GF_PATH_NAME);

   ident_t label = tree_ident(t), first = NULL;

   for (int64_t i = low; i <= high; i++) {
      ident_t id = ident_sprintf("%s(%"PRIi64")", istr(label), i);
      ident_t ndotted = ident_prefix(ctx->dotted, id, '.');

      tree_t b = tree_new(T_BLOCK);
      tree_set_ident(b, id);
      tree_set_loc(b, tree_loc(t));

      tree_add_stmt(ctx->out, b);

      tree_t g = tree_decl(t, 0);
      assert(tree_kind(g) == T_GENERIC_DECL);

      tree_t map = tree_new(T_PARAM);
      tree_set_subkind(map, P_POS);
      tree_set_loc(map, tree_loc(g));
      tree_set_value(map, get_int_lit(g, NULL, i));

      tree_add_generic(b, g);
      tree_add_genmap(b, map);

      elab_ctx_t new_ctx = {
         .out     = b,
         .dotted  = ndotted,
      };
      elab_inherit_context(&new_ctx, ctx);

      if (i == low)
         first = ndotted;
      else
         new_ctx.cloned = first;

      elab_cover_block(&new_ctx, t);
      elab_push_scope(t, &new_ctx);

      if (elab_new_errors(&new_ctx) == 0) {
         elab_decls(t, &new_ctx);
         elab_vhdl_processes(t, &new_ctx);
      }

      if (elab_new_errors(&new_ctx) == 0) {
         vhdl_cover_block(b, new_ctx.cover, new_ctx.cscope);
         elab_lower(b, &new_ctx);
         elab_vhdl_sub_blocks(t, &new_ctx);
      }

      elab_pop_scope(&new_ctx);
   }
}

static bool elab_generate_test(tree_t value, const elab_ctx_t *ctx)
{
   bool test;
   if (folded_bool(value, &test))
      return test;

   tree_t folded = elab_eval_expr(value, ctx);

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

         ident_t name = tree_ident(cond);
         ident_t ndotted = ident_prefix(ctx->dotted, name, '.');

         elab_ctx_t new_ctx = {
            .out    = b,
            .dotted = ndotted,
         };
         elab_inherit_context(&new_ctx, ctx);

         elab_cover_block(&new_ctx, t);
         elab_push_scope(t, &new_ctx);
         elab_decls(cond, &new_ctx);
         elab_vhdl_processes(cond, &new_ctx);

         if (error_count() == 0) {
            vhdl_cover_block(b, new_ctx.cover, new_ctx.cscope);
            elab_lower(b, &new_ctx);
            elab_vhdl_sub_blocks(cond, &new_ctx);
         }

         elab_pop_scope(&new_ctx);
         return;
      }
   }
}

static void elab_case_generate(tree_t t, const elab_ctx_t *ctx)
{
   void *context = *mptr_get(ctx->scope->privdata);
   tree_t chosen = eval_case(ctx->jit, t, ctx->lowered, context);
   if (chosen == NULL)
      return;

   ident_t id = tree_has_ident(chosen) ? tree_ident(chosen) : tree_ident(t);

   tree_t b = tree_new(T_BLOCK);
   tree_set_loc(b, tree_loc(chosen));
   tree_set_ident(b, id);

   tree_add_stmt(ctx->out, b);

   ident_t ndotted = ident_prefix(ctx->dotted, id, '.');

   elab_ctx_t new_ctx = {
      .out    = b,
      .dotted = ndotted,
   };
   elab_inherit_context(&new_ctx, ctx);

   elab_cover_block(&new_ctx, t);
   elab_push_scope(t, &new_ctx);
   elab_decls(chosen, &new_ctx);
   elab_vhdl_processes(chosen, &new_ctx);

   if (error_count() == 0) {
      vhdl_cover_block(b, new_ctx.cover, new_ctx.cscope);
      elab_lower(b, &new_ctx);
      elab_vhdl_sub_blocks(chosen, &new_ctx);
   }

   elab_pop_scope(&new_ctx);
}

static void elab_vhdl_processes(tree_t t, const elab_ctx_t *ctx)
{
   const int nstmts = tree_stmts(t);
   for (int i = 0; i < nstmts; i++) {
      tree_t s = tree_stmt(t, i);

      switch (tree_kind(s)) {
      case T_PROCESS:
      case T_PSL_DIRECT:
         tree_add_stmt(ctx->out, s);
         break;
      default:
         break;
      }
   }
}

static void elab_vhdl_sub_blocks(tree_t t, const elab_ctx_t *ctx)
{
   const int nstmts = tree_stmts(t);
   for (int i = 0; i < nstmts; i++) {
      tree_t s = tree_stmt(t, i);

      switch (tree_kind(s)) {
      case T_INSTANCE:
         elab_instance(s, ctx);
         break;
      case T_BLOCK:
         elab_vhdl_block(s, ctx);
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
      case T_PSL_DIRECT:
         break;
      default:
         fatal_trace("unexpected statement %s", tree_kind_str(tree_kind(s)));
      }
   }
}

static void elab_vhdl_block(tree_t t, const elab_ctx_t *ctx)
{
   ident_t id = tree_ident(t);

   tree_t b = tree_new(T_BLOCK);
   tree_set_ident(b, id);
   tree_set_loc(b, tree_loc(t));

   tree_add_stmt(ctx->out, b);

   ident_t ndotted = ident_prefix(ctx->dotted, id, '.');

   elab_ctx_t new_ctx = {
      .out    = b,
      .dotted = ndotted,
   };
   elab_inherit_context(&new_ctx, ctx);

   elab_cover_block(&new_ctx, t);
   elab_push_scope(t, &new_ctx);
   elab_generics(t, t, &new_ctx);
   elab_ports(t, t, &new_ctx);
   elab_decls(t, &new_ctx);
   elab_vhdl_processes(t, &new_ctx);

   if (elab_new_errors(&new_ctx) == 0) {
      vhdl_cover_block(b, new_ctx.cover, new_ctx.cscope);
      elab_lower(b, &new_ctx);
      elab_vhdl_sub_blocks(t, &new_ctx);
   }

   elab_pop_scope(&new_ctx);
}

static tree_t elab_top_level_binding(tree_t arch, const elab_ctx_t *ctx)
{
   tree_t inst = tree_new(T_INSTANCE);
   tree_set_ident(inst, ident_rfrom(tree_ident(tree_primary(arch)), '.'));
   tree_set_loc(inst, tree_loc(arch));
   tree_set_ref(inst, arch);
   tree_set_class(inst, C_ARCHITECTURE);

   tree_t entity = tree_primary(arch);
   const int ngenerics = tree_generics(entity);

   for (int i = 0; i < ngenerics; i++) {
      tree_t g = tree_generic(entity, i);
      ident_t name = tree_ident(g);

      if (tree_flags(g) & TREE_F_PREDEFINED)
         continue;    // Predefined generic subprograms
      else if (tree_class(g) != C_CONSTANT) {
         error_at(tree_loc(g), "only constant top-level generics are "
                  "supported");
         continue;
      }

      tree_t value = elab_find_generic_override(g, ctx);
      if (value == NULL && tree_has_value(g))
         value = tree_value(g);
      else if (value == NULL) {
         error_at(tree_loc(g), "generic %pI of top-level entity must have "
                  "default value or be specified using -gNAME=VALUE", name);
         continue;
      }

      tree_t map = tree_new(T_PARAM);
      tree_set_subkind(map, P_POS);
      tree_set_pos(map, i);
      tree_set_value(map, value);

      tree_add_genmap(inst, map);
   }

   const int nports = tree_ports(entity);
   for (int i = 0; i < nports; i++) {
      tree_t p = tree_port(entity, i);

      tree_t m = tree_new(T_PARAM);
      tree_set_subkind(m, P_POS);
      tree_set_pos(m, i);

      if (tree_subkind(p) == PORT_IN && tree_has_value(p))
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

      tree_add_param(inst, m);
   }

   return inst;
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

static void elab_vhdl_root_cb(void *arg)
{
   elab_ctx_t *ctx = arg;

   tree_t vhdl = tree_from_object(ctx->root);
   assert(vhdl != NULL);

   // vlog_deferred is owned by the synthetic root (elab() sets it up
   // once); per-top root_cbs just inherit it via elab_inherit_context
   // so the post-elab resolver sees every Verilog subtree regardless
   // of which top introduced it.

   tree_t arch = vhdl;
   switch (tree_kind(vhdl)) {
   case T_ENTITY:
      arch = elab_pick_arch(vhdl, &ctx->root->loc, ctx);
      // Fall-through
   case T_ARCH:
      {
         tree_t bind = elab_top_level_binding(arch, ctx);

         if (error_count() == 0)
            elab_architecture(bind, arch, ctx);
      }
      break;
   case T_CONFIGURATION:
      {
         tree_t config = tree_decl(vhdl, 0);
         assert(tree_kind(config) == T_BLOCK_CONFIG);

         tree_t inst = elab_top_level_binding(tree_ref(config), ctx);

         if (error_count() == 0)
            elab_configuration(inst, vhdl, ctx);
      }
      break;
   default:
      fatal("%s is not a suitable top-level unit", istr(tree_ident(vhdl)));
   }
}

// Entry point: resolve hier-refs across the full instance tree.
// Iterates ALL deferred bodies (module instances + generate block
// iterations + named blocks).  For each body, constructs the
// parent context chain by looking up ancestors in the deferred list.
static void elab_resolve_all_vlog_hier_refs(const elab_ctx_t *ctx)
{
   if (ctx->vlog_deferred == NULL || ctx->vlog_deferred->count == 0)
      return;

   const int count = ctx->vlog_deferred->count;
   vlog_deferred_body_t *items = ctx->vlog_deferred->items;

#ifdef DEBUG
   // Phase 2 coverage check: every Verilog body deferred for hier-ref
   // resolution must already be in the language-neutral scope tree.
   // If this fires, some elab path creates scopes without going
   // through elab_push_scope, and Phase 3's cross-language walk will
   // have blind spots.
   if (ctx->scope_tree != NULL) {
      for (int i = 0; i < count; i++)
         assert(hash_get(ctx->scope_tree, items[i].dotted) != NULL);
   }
#endif

   // Process each Verilog body.  Build the parent context chain from
   // scope_tree so VHDL ancestors (which don't appear in the Verilog
   // deferred list) are included — the resolver's cross-language
   // walk needs them as valid probe points for upward searches.
   for (int i = 0; i < count; i++) {
      vlog_deferred_body_t *db = &items[i];

      hier_node_t *leaf =
         ctx->scope_tree ? hash_get(ctx->scope_tree, db->dotted) : NULL;

      // Collect ancestors leaf-first.  If scope_tree is unavailable,
      // fall back to the deferred body alone — older paths that
      // bypass scope_tree still resolve within their own body.
      SCOPED_A(hier_node_t *) ancestors = AINIT;
      for (hier_node_t *cur = leaf; cur != NULL; cur = cur->parent)
         APUSH(ancestors, cur);

      const int depth = ancestors.count;
      if (depth == 0) {
         // No scope_tree entry — synthesize a one-element chain.
         elab_ctx_t tmp = {};
         elab_inherit_context(&tmp, ctx);
         tmp.parent     = NULL;
         tmp.dotted     = db->dotted;
         tmp.inst_alias = db->inst_alias;
         tmp.cloned     = vlog_ident(db->body);
         tmp.vlog_body  = db->body;
         elab_resolve_vlog_hier_refs(db->body, &tmp);
         continue;
      }

      // Build elab_ctx chain from root (index depth-1) to leaf (0).
      elab_ctx_t *chain LOCAL = xmalloc_array(depth, sizeof(elab_ctx_t));
      for (int d = depth - 1; d >= 0; d--) {
         hier_node_t *n = ancestors.items[d];
         elab_ctx_t tmp = {};
         elab_inherit_context(&tmp, ctx);
         tmp.parent     = (d < depth - 1) ? &chain[d + 1] : NULL;
         tmp.dotted     = n->ids.dotted;
         tmp.inst_alias = n->ids.inst_alias;
         tmp.cloned     = n->ids.cloned;
         tmp.vlog_body  = n->vlog_body;
         chain[d] = tmp;
      }

      elab_resolve_vlog_hier_refs(db->body, &chain[0]);
   }
}

static void elab_verilog_root_cb(void *arg)
{
   elab_ctx_t *ctx = arg;

   vlog_node_t vlog = vlog_from_object(ctx->root);
   assert(vlog != NULL);

   ident_t label = vlog_ident2(vlog);

   vlog_node_t stmt = vlog_new(V_MOD_INST);
   vlog_set_ident(stmt, label);

   vlog_node_t list = vlog_new(V_INST_LIST);
   vlog_set_ident(list, label);
   vlog_add_stmt(list, stmt);
   vlog_set_loc(list, vlog_loc(vlog));

   // vlog_deferred is owned by the synthetic root; see elab_vhdl_root_cb.
   elab_verilog_module(NULL, label, vlog, list, stmt, ctx);
}

static int elab_compar_modcache(const void *a, const void *b)
{
   const mod_cache_t *mc_a = a, *mc_b = b;

   if (mc_b->count < mc_a->count)
      return -1;
   else if (mc_b->count > mc_a->count)
      return 1;
   else
      return 0;
}

static void elab_print_stats(const elab_ctx_t *ctx)
{
   const unsigned count = hash_members(ctx->modcache);
   mod_cache_t *sorted LOCAL = xmalloc_array(count, sizeof(mod_cache_t));

   int pos = 0;
   const void *key;
   mod_cache_t *mc;
   for (hash_iter_t it = HASH_BEGIN;
        hash_iter(ctx->modcache, &it, &key, (void *)&mc); )
      sorted[pos++] = *mc;
   assert(pos == count);

   qsort(sorted, count, sizeof(mod_cache_t), elab_compar_modcache);

   nvc_printf("\n$bold$%-50s %10s %10s$$\n", "Design Unit", "Instances",
              "Unique");

   for (int i = 0; i < count; i++) {
      ident_t name = NULL;

      tree_t vhdl = tree_from_object(sorted[i].object);
      if (vhdl != NULL)
         name = tree_ident(vhdl);

      vlog_node_t vlog = vlog_from_object(sorted[i].object);
      if (vlog != NULL)
         name = vlog_ident(vlog);

      printf("%-50s %10d %10d\n", istr(name), sorted[i].count,
             sorted[i].unique);
   }

   printf("\n");
}

tree_t elab(object_t **tops, int ntops, jit_t *jit, unit_registry_t *ur,
            mir_context_t *mc, cover_data_t *cover, sdf_file_t *sdf,
            rt_model_t *m)
{
   make_new_arena();

   if (ntops < 1)
      fatal("elaboration requires at least one top-level unit");

   // Primary top names the T_ELAB unit (backward-compatible naming).
   object_t *primary = tops[0];
   ident_t name = NULL;

   tree_t vhdl0 = tree_from_object(primary);
   if (vhdl0 != NULL)
      name = ident_prefix(tree_ident(vhdl0), well_known(W_ELAB), '.');

   vlog_node_t vlog0 = vlog_from_object(primary);
   if (vlog0 != NULL)
      name = ident_prefix(vlog_ident(vlog0), well_known(W_ELAB), '.');

   if (vhdl0 == NULL && vlog0 == NULL)
      fatal("top level is not a VHDL design unit or Verilog module");

   tree_t e = tree_new(T_ELAB);
   tree_set_ident(e, name);
   tree_set_loc(e, &(primary->loc));

   lib_t work = lib_work();

   // Synthetic anonymous root — the anchor for $root walks and the
   // common parent of every sibling top.  Carries bootstrap state;
   // each per-top context inherits from it.  vlog_deferred lives
   // here so a single post-elab resolver pass sees every subtree.
   vlog_deferred_list_t deferred = {};
   elab_ctx_t root_ctx = {
      .is_synthetic_root = true,
      .out               = e,
      .cover             = cover,
      .library           = work,
      .jit               = jit,
      .sdf               = sdf,
      .registry          = ur,
      .mir               = mc,
      .modcache          = hash_new(16),
      .blockcache        = hash_new(16),
      .mracache          = hash_new(16),
      .scope_tree        = hash_new(64),
      .dotted            = lib_name(work),
      .model             = m,
      .scope             = create_scope(m, e, NULL),
      .pool              = pool_new(),
      .vlog_deferred     = &deferred,
   };

   // Elaborate each top as a child of the synthetic root.  Per-top
   // context inherits everything substantive from the root; the
   // only per-top setup is pointing ->root at this iteration's
   // object so the language-specific callback picks it up.
   for (int i = 0; i < ntops; i++) {
      object_t *this_top = tops[i];
      tree_t this_vhdl = tree_from_object(this_top);
      vlog_node_t this_vlog = vlog_from_object(this_top);
      if (this_vhdl == NULL && this_vlog == NULL)
         fatal("top level is not a VHDL design unit or Verilog module");

      elab_ctx_t top_ctx = {};
      elab_inherit_context(&top_ctx, &root_ctx);
      top_ctx.scope = root_ctx.scope;
      top_ctx.root  = this_top;

      if (this_vhdl != NULL)
         call_with_model(m, elab_vhdl_root_cb, &top_ctx);
      else
         call_with_model(m, elab_verilog_root_cb, &top_ctx);

      if (error_count() > 0)
         break;
   }

   // Resolve all Verilog hier-refs across the whole design in one
   // pass so cross-top refs (e.g. $root.glbl.GSR from within a
   // design top) can see their siblings.
   elab_resolve_all_vlog_hier_refs(&root_ctx);

   ACLEAR(deferred);
   root_ctx.vlog_deferred = NULL;

   if (opt_get_int(OPT_ELAB_STATS))
      elab_print_stats(&root_ctx);

   const void *key;
   void *value;
   for (hash_iter_t it = HASH_BEGIN;
        hash_iter(root_ctx.modcache, &it, &key, &value); )
      ghash_free(((mod_cache_t *)value)->instances);

   hash_free(root_ctx.modcache);
   hash_free(root_ctx.blockcache);
   hash_free(root_ctx.mracache);
   hash_free(root_ctx.scope_tree);
   pool_free(root_ctx.pool);

   if (error_count() > 0)
      return NULL;

   if (opt_get_verbose(OPT_ELAB_VERBOSE, NULL))
      dump(e);

   for (generic_list_t *it = generic_override; it != NULL; it = it->next)
      warnf("generic value for %s not used", istr(it->name));

   ident_t b0_name = tree_ident(tree_stmt(e, 0));
   ident_t vu_name = ident_prefix(lib_name(root_ctx.library), b0_name, '.');
   unit_registry_flush(ur, vu_name);

   freeze_global_arena();
   return e;
}

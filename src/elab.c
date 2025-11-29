//
//  Copyright (C) 2011-2025  Nick Gasson
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
#include "driver.h"
#include "eval.h"
#include "hash.h"
#include "inst.h"
#include "lib.h"
#include "lower.h"
#include "mask.h"
#include "object.h"
#include "option.h"
#include "phase.h"
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

typedef struct _elab_ctx elab_ctx_t;
typedef struct _generic_list generic_list_t;

typedef struct _elab_ctx {
   const elab_ctx_t *parent;
   tree_t            out;
   object_t         *root;
   tree_t            inst;
   ident_t           dotted;
   lib_t             library;
   jit_t            *jit;
   unit_registry_t  *registry;
   mir_context_t    *mir;
   lower_unit_t     *lowered;
   cover_data_t     *cover;
   sdf_file_t       *sdf;
   driver_set_t     *drivers;
   hash_t           *modcache;
   hash_t           *mracache;
   rt_model_t       *model;
   rt_scope_t       *scope;
   mem_pool_t       *pool;
   unsigned          depth;
   unsigned          errors;
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
   vlog_node_t body;
   tree_t      block;
   tree_t      wrap;
} elab_instance_t;

static void elab_block(tree_t t, const elab_ctx_t *ctx);
static void elab_stmts(tree_t t, const elab_ctx_t *ctx);
static void elab_decls(tree_t t, const elab_ctx_t *ctx);
static void elab_push_scope(tree_t t, elab_ctx_t *ctx);
static void elab_pop_scope(elab_ctx_t *ctx);
static void elab_verilog_stmts(vlog_node_t v, const elab_ctx_t *ctx);

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

static elab_instance_t *elab_new_instance(vlog_node_t mod, vlog_node_t inst,
                                          const elab_ctx_t *ctx)
{
   assert(is_top_level(mod));

   mod_cache_t *mc = elab_cached_module(vlog_to_object(mod), ctx);
   mc->count++;

   elab_instance_t *ei = ghash_get(mc->instances, inst);
   if (ei != NULL)
      return ei;

   ei = pool_calloc(ctx->pool, sizeof(elab_instance_t));
   ei->body = vlog_new_instance(mod, inst, ctx->dotted);

   ei->wrap = tree_new(T_VERILOG);
   tree_set_loc(ei->wrap, vlog_loc(mod));
   tree_set_ident(ei->wrap, vlog_ident(mod));
   tree_set_vlog(ei->wrap, ei->body);

   ei->block = tree_new(T_BLOCK);
   tree_set_loc(ei->block, vlog_loc(inst));
   tree_set_ident(ei->block, vlog_ident(inst));

   vlog_trans(ei->body, ei->block);

   ghash_put(mc->instances, inst, ei);
   mc->unique++;

   return ei;
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
        *it && (*it)->name != name;
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
           *it && (*it)->name != qual;
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
         elab_map_generic_type(tree_type(g), tree_type(value), map);
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
            for (int i = 0; i < ngenerics; i++) {
               tree_t fg = tree_generic(formal, i);
               tree_t ag = tree_generic(actual, i);

               switch (tree_class(fg)) {
               case C_FUNCTION:
               case C_PROCEDURE:
                  {
                     // Get the actual subprogram from the generic map
                     assert(ngenerics == tree_genmaps(actual));
                     tree_t ref = tree_value(tree_genmap(actual, i));
                     assert(tree_kind(ref) == T_REF);

                     hash_put(map, fg, tree_ref(ref));
                  }
                  break;
               case C_TYPE:
                  hash_put(map, tree_type(fg), tree_type(ag));
                  break;
               case C_PACKAGE:
                  // TODO: this should be processed recursively
               default:
                  hash_put(map, fg, ag);
                  break;
               }
            }

            hash_put(map, g, actual);
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
         {
            tree_t formal = tree_ref(tree_value(g));
            tree_t actual = tree_ref(value);

            const int ndecls = tree_decls(formal);
            for (int i = 0; i < ndecls; i++) {
               tree_t gd = tree_decl(formal, i);
               tree_t ad = tree_decl(actual, i);
               assert(tree_kind(gd) == tree_kind(ad));

               hash_put(fixup, gd, ad);

               if (is_type_decl(gd))
                  hash_put(fixup, tree_type(gd), tree_type(ad));
            }

            const int ngenerics = tree_generics(formal);
            for (int i = 0; i < ngenerics; i++) {
               tree_t fg = tree_generic(formal, i);
               tree_t ag = tree_generic(actual, i);

               switch (tree_class(fg)) {
               case C_FUNCTION:
               case C_PROCEDURE:
                  {
                     // Get the actual subprogram from the generic map
                     assert(ngenerics == tree_genmaps(actual));
                     tree_t ref = tree_value(tree_genmap(actual, i));
                     assert(tree_kind(ref) == T_REF);

                     hash_put(fixup, fg, tree_ref(ref));
                  }
                  break;
               case C_TYPE:
                  hash_put(fixup, tree_type(fg), tree_type(ag));
                  break;
               case C_PACKAGE:
                  // TODO: this should be processed recursively
               default:
                  hash_put(fixup, fg, ag);
                  break;
               }
            }

            hash_put(fixup, g, actual);
         }
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

static void elab_inherit_context(elab_ctx_t *ctx, const elab_ctx_t *parent)
{
   ctx->parent   = parent;
   ctx->jit      = parent->jit;
   ctx->registry = parent->registry;
   ctx->mir      = parent->mir;
   ctx->root     = parent->root;
   ctx->dotted   = ctx->dotted ?: parent->dotted;
   ctx->library  = ctx->library ?: parent->library;
   ctx->out      = ctx->out ?: parent->out;
   ctx->cover    = parent->cover;
   ctx->sdf      = parent->sdf;
   ctx->inst     = ctx->inst ?: parent->inst;
   ctx->modcache = parent->modcache;
   ctx->mracache = parent->mracache;
   ctx->depth    = parent->depth + 1;
   ctx->model    = parent->model;
   ctx->errors   = error_count();
   ctx->pool     = parent->pool;
}

static bool elab_new_errors(const elab_ctx_t *ctx)
{
   return error_count() - ctx->errors;
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

static void elab_lower(tree_t b, elab_ctx_t *ctx)
{
   tree_t hier = tree_decl(b, 0);
   assert(tree_kind(hier) == T_HIER);

   if (tree_subkind(hier) == T_VERILOG)
      vlog_lower_block(ctx->mir, ctx->parent->dotted, b);
   else
      ctx->lowered = lower_instance(ctx->registry, ctx->parent->lowered,
                                    ctx->cover, b);

   if (ctx->inst != NULL)
      diag_add_hint_fn(elab_hint_fn, ctx->inst);

   ctx->scope = create_scope(ctx->model, b, ctx->parent->scope);

   if (ctx->inst != NULL)
      diag_remove_hint_fn(elab_hint_fn);
}

static void elab_verilog_module(tree_t bind, ident_t label,
                                const elab_instance_t *ei,
                                const elab_ctx_t *ctx)
{
   ident_t ndotted = ident_prefix(ctx->dotted, label, '.');

   elab_ctx_t new_ctx = {
      .dotted    = ndotted,
   };
   elab_inherit_context(&new_ctx, ctx);

   tree_t b = tree_new(T_BLOCK);
   tree_set_ident(b, label);
   tree_set_loc(b, tree_loc(ctx->out));

   tree_add_stmt(ctx->out, b);
   new_ctx.out = b;

   elab_push_scope(ei->wrap, &new_ctx);

   if (bind != NULL)
      elab_ports(ei->block, bind, &new_ctx);

   if (elab_new_errors(&new_ctx) == 0)
      elab_decls(ei->block, &new_ctx);

   if (elab_new_errors(&new_ctx) == 0) {
      new_ctx.drivers = find_drivers(ei->block);
      elab_lower(b, &new_ctx);
   }

   if (elab_new_errors(&new_ctx) == 0)
      elab_verilog_stmts(ei->body, &new_ctx);

   elab_pop_scope(&new_ctx);
}

static void elab_verilog_ports(vlog_node_t inst, elab_instance_t *ei,
                               const elab_ctx_t *ctx)
{
   assert(vlog_kind(inst) == V_MOD_INST);

   const int nports = vlog_ports(ei->body);
   const int nparams = vlog_params(inst);

   if (nports != nparams) {
      error_at(vlog_loc(inst), "expected %d port connections for module %s "
               "but found %d", nports, istr(vlog_ident2(ei->body)), nparams);
      return;
   }

   ident_t block_name = vlog_ident(inst);

   for (int i = 0; i < nports; i++) {
      vlog_node_t port = vlog_ref(vlog_port(ei->body, i));
      ident_t port_name = vlog_ident(port);

      vlog_node_t conn = vlog_param(inst, i);
      assert(vlog_kind(conn) == V_PORT_CONN);

      if (vlog_has_ident(conn) && vlog_ident(conn) != port_name) {
         bool found = false;
         for (int j = 0; j < nparams && !found; j++) {
            if (vlog_ident((conn = vlog_param(inst, j))) == port_name)
               found = true;
         }

         if (!found) {
            diag_t *d = diag_new(DIAG_ERROR, vlog_loc(inst));
            diag_printf(d, "missing port connection for '%s'", istr(port_name));
            diag_hint(d, vlog_loc(port), "'%s' declared here", istr(port_name));
            diag_emit(d);
            continue;
         }
      }

      const loc_t *loc = vlog_loc(conn);
      ident_t name = ident_uniq("#assign#%s.%s", istr(block_name),
                                istr(port_name));

      vlog_node_t ref = vlog_new(V_HIER_REF);
      vlog_set_ident(ref, port_name);
      vlog_set_ref(ref, port);
      vlog_set_ident2(ref, block_name);

      vlog_node_t assign = vlog_new(V_ASSIGN), target;
      vlog_set_ident(assign, name);

      switch (vlog_subkind(port)) {
      case V_PORT_INPUT:
         vlog_set_target(assign, (target = ref));
         vlog_set_value(assign, vlog_value(conn));
         break;
      case V_PORT_OUTPUT:
         vlog_set_target(assign, (target = vlog_value(conn)));
         vlog_set_value(assign, ref);
         break;
      default:
         fatal_at(vlog_loc(port), "sorry, this port kind is not supported");
      }

      if (!vlog_is_net(target)) {
         diag_t *d = diag_new(DIAG_ERROR, vlog_loc(conn));
         if (vlog_kind(target) == V_REF)
            diag_printf(d, "'%s'", istr(vlog_ident(target)));
         else
            diag_printf(d, "expression");
         diag_printf(d, " cannot be driven by continuous assignment from "
                     "port '%s'", istr(vlog_ident(port)));
         diag_emit(d);
      }

      vlog_set_loc(assign, loc);

      tree_t wrap = tree_new(T_VERILOG);
      tree_set_ident(wrap, name);
      tree_set_vlog(wrap, assign);
      tree_set_loc(wrap, loc);

      tree_add_stmt(ctx->out, wrap);

      ident_t sym = ident_prefix(ctx->dotted, name, '.');

      mir_defer(ctx->mir, sym, ctx->dotted, MIR_UNIT_PROCESS,
                vlog_lower_deferred, vlog_to_object(assign));
   }
}

static void elab_verilog_instance_list(vlog_node_t v, const elab_ctx_t *ctx)
{
   ident_t modname = vlog_ident(v);
   ident_t libname = lib_name(ctx->library);

   text_buf_t *tb = tb_new();
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

   elab_instance_t *ei = elab_new_instance(mod, v, ctx);

   const int nstmts = vlog_stmts(v);
   for (int i = 0; i < nstmts; i++) {
      vlog_node_t inst = vlog_stmt(v, i);
      assert(vlog_kind(inst) == V_MOD_INST);

      elab_verilog_module(NULL, vlog_ident(inst), ei, ctx);
      elab_verilog_ports(inst, ei, ctx);
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

   tree_t wrap = tree_new(T_VERILOG);
   tree_set_ident(wrap, id);
   tree_set_loc(wrap, vlog_loc(v));
   tree_set_vlog(wrap, v);

   elab_push_scope(wrap, &new_ctx);

   vlog_trans(v, b);

   elab_lower(b, &new_ctx);
   elab_verilog_stmts(v, &new_ctx);

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

static void elab_verilog_for_generate(vlog_node_t v, const elab_ctx_t *ctx)
{
   mir_unit_t *init = vlog_lower_thunk(ctx->mir, ctx->dotted, vlog_left(v));
   mir_unit_t *test = vlog_lower_thunk(ctx->mir, ctx->dotted, vlog_value(v));
   mir_unit_t *step = vlog_lower_thunk(ctx->mir, ctx->dotted, vlog_right(v));

   void *context = *mptr_get(ctx->scope->privdata);

   jit_call_thunk2(ctx->jit, init, context, NULL, NULL);

   vlog_node_t genvar = vlog_ref(vlog_target(vlog_stmt(vlog_left(v), 0)));
   assert(vlog_kind(genvar) == V_GENVAR_DECL);

   vlog_node_t ref = vlog_new(V_REF);
   vlog_set_ref(ref, genvar);
   vlog_set_loc(ref, vlog_loc(genvar));

   mir_unit_t *get_var = vlog_lower_thunk(ctx->mir, ctx->dotted, ref);

   vlog_node_t s0 = vlog_stmt(v, 0);
   assert(vlog_kind(s0) == V_BLOCK);

   while (jit_call_thunk2(ctx->jit, test, context,
                          elab_verilog_for_generate_test_cb, NULL)) {
      int32_t index;
      jit_call_thunk2(ctx->jit, get_var, context,
                      elab_verilog_for_generate_index_cb, &index);

      vlog_node_t copy = vlog_generate_instance(s0, genvar, index, ctx->dotted);

      elab_verilog_block(copy, ctx);

      jit_call_thunk2(ctx->jit, step, context, NULL, NULL);
   }

   mir_unit_free(get_var);
   mir_unit_free(init);
   mir_unit_free(test);
   mir_unit_free(step);
}

static void elab_verilog_stmts(vlog_node_t v, const elab_ctx_t *ctx)
{
   const int nstmts = vlog_stmts(v);
   for (int i = 0; i < nstmts; i++) {
      vlog_node_t s = vlog_stmt(v, i);

      const vlog_kind_t kind = vlog_kind(s);
      switch (kind) {
      case V_INST_LIST:
         elab_verilog_instance_list(s, ctx);
         break;
      case V_INITIAL:
      case V_ALWAYS:
      case V_ASSIGN:
      case V_UDP_TABLE:
      case V_GATE_INST:
         {
            ident_t name = vlog_ident(s);
            ident_t sym = ident_prefix(ctx->dotted, name, '.');

            tree_t w = tree_new(T_VERILOG);
            tree_set_ident(w, name);
            tree_set_loc(w, vlog_loc(s));
            tree_set_vlog(w, s);

            tree_add_stmt(ctx->out, w);

            if (kind == V_UDP_TABLE)
               mir_defer(ctx->mir, sym, ctx->dotted, MIR_UNIT_PROCESS,
                         vlog_lower_udp, vlog_to_object(v));
            else
               mir_defer(ctx->mir, sym, ctx->dotted, MIR_UNIT_PROCESS,
                         vlog_lower_deferred, vlog_to_object(s));
         }
         break;
      case V_BLOCK:
         elab_verilog_block(s, ctx);
         break;
      case V_IF_GENERATE:
         error_at(vlog_loc(s), "if-generate construct could not be "
                  "evaluated at elaboration time");
         break;
      case V_FOR_GENERATE:
         elab_verilog_for_generate(s, ctx);
         break;
      case V_SPECIFY:
         warn_at(vlog_loc(s), "specify blocks are not currently supported and "
                 "will be ignored");
         break;
      default:
         fatal_at(vlog_loc(s), "sorry, this Verilog statement is not "
                  "currently supported");
      }
   }
}

static vlog_node_t elab_mixed_generics(tree_t comp, vlog_node_t mod,
                                       const elab_ctx_t *ctx)
{
   vlog_node_t stmt = vlog_new(V_MOD_INST);
   vlog_set_ident(stmt, vlog_ident2(mod));

   vlog_node_t list = vlog_new(V_INST_LIST);
   vlog_set_ident(list, vlog_ident2(mod));
   vlog_add_stmt(list, stmt);

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
   elab_instance_fixup(comp, &new_ctx);
   elab_ports(comp, inst, &new_ctx);

   if (elab_new_errors(&new_ctx) == 0)
      elab_lower(b, &new_ctx);

   vlog_node_t list = elab_mixed_generics(comp, mod, &new_ctx);
   if (list != NULL) {
      elab_instance_t *ei = elab_new_instance(mod, list, &new_ctx);

      tree_t bind = elab_mixed_binding(comp, ei);
      if (bind != NULL && elab_new_errors(&new_ctx) == 0)
         elab_verilog_module(bind, vlog_ident2(mod), ei, &new_ctx);
   }

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

static void elab_architecture(tree_t inst, tree_t arch, const elab_ctx_t *ctx)
{
   ident_t label = tree_ident(inst);
   ident_t ndotted = ident_prefix(ctx->dotted, label, '.');

   elab_ctx_t new_ctx = {
      .dotted = ndotted,
      .inst   = ctx->parent != NULL ? inst : NULL,
   };
   elab_inherit_context(&new_ctx, ctx);

   tree_t b = tree_new(T_BLOCK);
   tree_set_ident(b, label);
   tree_set_loc(b, tree_loc(inst));

   tree_add_stmt(ctx->out, b);
   new_ctx.out = b;

   new_ctx.library = lib_require(ident_until(tree_ident(arch), '.'));

   mod_cache_t *mc = elab_cached_module(tree_to_object(arch), &new_ctx);
   mc->count++;

   elab_instance_t *ei = ghash_get(mc->instances, inst);
   if (ei == NULL) {
      ei = pool_calloc(ctx->pool, sizeof(elab_instance_t));
      ei->block = vhdl_architecture_instance(arch, inst, new_ctx.dotted);

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

   if (error_count() == 0) {
      new_ctx.drivers = find_drivers(ei->block);
      elab_lower(b, &new_ctx);
      elab_stmts(ei->block, &new_ctx);
   }

   elab_pop_scope(&new_ctx);
}

static void elab_configuration(tree_t inst, tree_t unit, const elab_ctx_t *ctx)
{
   ident_t label = tree_ident(inst);
   ident_t ndotted = ident_prefix(ctx->dotted, label, '.');

   elab_ctx_t new_ctx = {
      .dotted = ndotted,
      .inst   = ctx->parent != NULL ? inst : NULL,
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

   mod_cache_t *mc = elab_cached_module(tree_to_object(unit), &new_ctx);
   mc->count++;

   elab_instance_t *ei = ghash_get(mc->instances, inst);
   if (ei == NULL) {
      ei = pool_calloc(ctx->pool, sizeof(elab_instance_t));
      ei->block = vhdl_config_instance(config, inst, new_ctx.dotted);

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

   if (error_count() == 0) {
      new_ctx.drivers = find_drivers(ei->block);
      elab_lower(b, &new_ctx);
      elab_stmts(ei->block, &new_ctx);
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
   if (ei == NULL) {
      ei = pool_calloc(ctx->pool, sizeof(elab_instance_t));
      ei->block = vhdl_component_instance(comp, inst, ctx->dotted);

      elab_fold_generics(ei->block, ctx);

      ghash_put(mc->instances, inst, ei);
      mc->unique++;
   }

   tree_t b = tree_new(T_BLOCK);
   tree_set_ident(b, tree_ident(inst));
   tree_set_loc(b, tree_loc(inst));

   tree_add_stmt(ctx->out, b);
   new_ctx.out = b;

   elab_push_scope(comp, &new_ctx);
   elab_generics(ei->block, inst, &new_ctx);
   elab_ports(ei->block, inst, &new_ctx);

   if (error_count() == 0) {
      new_ctx.drivers = find_drivers(ei->block);
      elab_lower(b, &new_ctx);
   }

   if (elab_new_errors(&new_ctx) == 0)
      elab_stmts(ei->block, &new_ctx);

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
   tree_set_ident2(h, ctx->dotted);

   tree_add_decl(ctx->out, h);
}

static void elab_pop_scope(elab_ctx_t *ctx)
{
   if (ctx->drivers != NULL)
      free_drivers(ctx->drivers);

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

   driver_set_t *ds = find_drivers(t);

   ident_t prefix = ident_prefix(ctx->dotted, tree_ident(t), '.');

   // Clone body and fold loop parameter if there is only a single instance
   const bool clone = low == high;

   for (int64_t i = low; i <= high; i++) {
      ident_t id = ident_sprintf("%s(%"PRIi64")", istr(tree_ident(t)), i);
      ident_t ndotted = ident_prefix(ctx->dotted, id, '.');

      tree_t b = tree_new(T_BLOCK);
      tree_set_ident(b, id);
      tree_set_loc(b, tree_loc(t));

      tree_add_stmt(ctx->out, b);

      tree_t body = clone ? vhdl_generate_instance(t, prefix, ndotted) : t;

      tree_t g = tree_decl(body, 0);
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
         .drivers = clone ? find_drivers(body) : ds,
      };
      elab_inherit_context(&new_ctx, ctx);

      elab_push_scope(t, &new_ctx);

      if (clone) {
         hash_t *h = hash_new(8);
         hash_put(h, g, tree_value(map));
         simplify_global(body, h, ctx->jit, ctx->registry, ctx->mir);
         hash_free(h);
      }

      if (elab_new_errors(&new_ctx) == 0)
         elab_decls(body, &new_ctx);

      if (elab_new_errors(&new_ctx) == 0) {
         elab_lower(b, &new_ctx);
         elab_stmts(body, &new_ctx);
      }

      if (!clone) new_ctx.drivers = NULL;
      elab_pop_scope(&new_ctx);
   }

   free_drivers(ds);
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

         elab_push_scope(t, &new_ctx);
         elab_decls(cond, &new_ctx);

         new_ctx.drivers = find_drivers(cond);

         if (error_count() == 0) {
            elab_lower(b, &new_ctx);
            elab_stmts(cond, &new_ctx);
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

   elab_push_scope(t, &new_ctx);
   elab_decls(chosen, &new_ctx);

   new_ctx.drivers = find_drivers(chosen);

   if (error_count() == 0) {
      elab_lower(b, &new_ctx);
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
      case T_PSL_DIRECT:
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

   ident_t ndotted = ident_prefix(ctx->dotted, id, '.');

   elab_ctx_t new_ctx = {
      .out    = b,
      .dotted = ndotted,
   };
   elab_inherit_context(&new_ctx, ctx);

   elab_push_scope(t, &new_ctx);
   elab_generics(t, t, &new_ctx);
   elab_ports(t, t, &new_ctx);
   elab_decls(t, &new_ctx);

   if (elab_new_errors(&new_ctx) == 0) {
      elab_lower(b, &new_ctx);
      elab_stmts(t, &new_ctx);
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
         error_at(tree_loc(g), "generic %s of top-level entity must have "
                  "default value or be specified using -gNAME=VALUE",
                  istr(name));
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

   elab_instance_t *ei = elab_new_instance(vlog, list, ctx);
   elab_verilog_module(NULL, label, ei, ctx);
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

   color_printf("\n$bold$%-50s %10s %10s$$\n", "Design Unit", "Instances",
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

tree_t elab(object_t *top, jit_t *jit, unit_registry_t *ur, mir_context_t *mc,
            cover_data_t *cover, sdf_file_t *sdf, rt_model_t *m)
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
      .root      = top,
      .cover     = cover,
      .library   = work,
      .jit       = jit,
      .sdf       = sdf,
      .registry  = ur,
      .mir       = mc,
      .modcache  = hash_new(16),
      .mracache  = hash_new(16),
      .dotted    = lib_name(work),
      .model     = m,
      .scope     = create_scope(m, e, NULL),
      .pool      = pool_new(),
   };

   if (vhdl != NULL)
      call_with_model(m, elab_vhdl_root_cb, &ctx);
   else
      call_with_model(m, elab_verilog_root_cb, &ctx);

   if (opt_get_int(OPT_ELAB_STATS))
      elab_print_stats(&ctx);

   const void *key;
   void *value;
   for (hash_iter_t it = HASH_BEGIN;
        hash_iter(ctx.modcache, &it, &key, &value); )
      ghash_free(((mod_cache_t *)value)->instances);

   hash_free(ctx.modcache);
   hash_free(ctx.mracache);
   pool_free(ctx.pool);

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
   return e;
}

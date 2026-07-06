//
//  Copyright (C) 2025-2026 Nick Gasson
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
#include "hash.h"
#include "ident.h"
#include "tree.h"
#include "type.h"
#include "phase.h"
#include "vhdl/vhdl-phase.h"

#include <assert.h>
#include <inttypes.h>
#include <stdlib.h>

typedef A(tree_t) tree_list_t;
typedef A(type_t) type_list_t;

typedef struct {
   tree_list_t copied_subs;
   type_list_t copied_types;
} copy_ctx_t;

static void collect_decls(tree_t t, object_copy_ctx_t *ctx);

static void tree_copy_cb(tree_t t, void *__ctx)
{
   copy_ctx_t *ctx = __ctx;

   if (is_subprogram(t))
      APUSH(ctx->copied_subs, t);
}

static void type_copy_cb(type_t type, void *__ctx)
{
   copy_ctx_t *ctx = __ctx;

   if (type_has_ident(type))
      APUSH(ctx->copied_types, type);
}

static void rename_mangled(copy_ctx_t *ctx, ident_t dotted,
                           const ident_t *prefixes, int nprefix)
{
   // Change the name of any copied types to reflect the new hiearchy
   for (int i = 0; i < ctx->copied_types.count; i++) {
      type_t type = ctx->copied_types.items[i];
      ident_t orig = type_ident(type);
      for (int j = 0; j < nprefix; j++) {
         if (ident_starts_with(orig, prefixes[j])) {
            LOCAL_TEXT_BUF tb = tb_new();
            tb_cat(tb, istr(dotted));
            tb_cat(tb, istr(orig) + ident_len(prefixes[j]));

            type_set_ident(type, ident_new(tb_get(tb)));
            break;
         }
      }
   }
   ACLEAR(ctx->copied_types);

   // Change the mangled name of copied subprograms so that copies in
   // different instances do not collide
   for (int i = 0; i < ctx->copied_subs.count; i++) {
      tree_t decl = ctx->copied_subs.items[i];
      if (tree_kind(decl) == T_GENERIC_DECL)
         continue;   // Does not yet have mangled name
      else if (is_open_coded_builtin(tree_subkind(decl)))
         continue;   // Has no mangled name

      ident_t orig = tree_ident2(decl);
      for (int j = 0; j < nprefix; j++) {
         if (ident_starts_with(orig, prefixes[j])) {
            ident_t prefix = ident_runtil(orig, '(');

            LOCAL_TEXT_BUF tb = tb_new();
            tb_cat(tb, istr(dotted));
            tb_cat(tb, istr(prefix) + ident_len(prefixes[j]));

            type_t type = tree_type(decl);
            const bool is_func = type_has_result(type);
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

            if (tree_flags(decl) & TREE_F_PREDEFINED)
               tb_cat(tb, "$predef");

            ident_t mangled = ident_new(tb_get(tb));
            tree_set_ident2(decl, mangled);

            break;
         }
      }
   }
   ACLEAR(ctx->copied_subs);
}

static void collect_generic_types(type_t type, object_copy_ctx_t *ctx)
{
   type_copy_mark(type, ctx);

   // Also collect any anonymous generic types
   switch (type_subkind(type)) {
   case GTYPE_ARRAY:
      {
         type_t elem = type_elem(type);
         if (type_kind(elem) == T_GENERIC && !type_has_ident(elem))
            collect_generic_types(elem, ctx);

         const int nindex = type_indexes(type);
         for (int i = 0; i < nindex; i++) {
            type_t index = type_index(type, i);
            if (type_kind(index) == T_GENERIC && !type_has_ident(index))
               collect_generic_types(index, ctx);
         }
      }
      break;

   default:
      break;
   }
}

static void collect_generics(tree_t t, object_copy_ctx_t *ctx)
{
   const int ngenerics = tree_generics(t);
   for (int i = 0; i < ngenerics; i++) {
      tree_t g = tree_generic(t, i);

      // If the uninstantiated unit has any package generics then we
      // need to copy those too in order to fix up the types
      switch (tree_class(g)) {
      case C_PACKAGE:
         {
            tree_t ref = tree_value(g);
            if (tree_has_ref(ref)) {
               tree_t pack = tree_ref(ref);
               assert(is_uninstantiated_package(pack));
               collect_generics(pack, ctx);
               collect_decls(pack, ctx);
            }
         }
         break;
      case C_TYPE:
         collect_generic_types(tree_type(g), ctx);
         break;
      case C_CONSTANT:
         tree_copy_mark(g, ctx);
         break;
      default:
         break;
      }
   }
}

static void collect_decls(tree_t t, object_copy_ctx_t *ctx)
{
   const int ndecls = tree_decls(t);
   for (int i = 0 ; i < ndecls; i++) {
      tree_t d = tree_decl(t, i);
      tree_copy_mark(d, ctx);

      switch (tree_kind(d)) {
      case T_PACKAGE:
         collect_generics(d, ctx);
         // Fall-through
      case T_PACK_BODY:
      case T_PACK_INST:
         collect_decls(d, ctx);
         break;
      case T_TYPE_DECL:
         type_copy_mark(type_base_recur(tree_type(d)), ctx);
         break;
      case T_PROT_DECL:
      case T_PROT_BODY:
         type_copy_mark(tree_type(d), ctx);
         collect_decls(d, ctx);
         break;
      default:
         break;
      }
   }
}

static void map_generics(tree_t out, tree_t entity, tree_t bind)
{
   const int ngenerics = tree_generics(entity);
   const int ngenmaps = tree_genmaps(bind);

   for (int i = 0; i < ngenerics; i++) {
      tree_t g = tree_generic(entity, i);
      tree_add_generic(out, g);

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

      if (map == NULL)
         error_at(tree_loc(bind), "missing value for generic %s with no "
                  "default", istr(tree_ident(g)));
      else
         tree_add_genmap(out, map);
   }
}

tree_t vhdl_component_instance(tree_t comp, tree_t inst, ident_t dotted)
{
   assert(tree_kind(comp) == T_COMPONENT);
   assert(tree_kind(inst) == T_INSTANCE);

   tree_t spec = tree_spec(inst);
   assert(tree_kind(spec) == T_SPEC);

   tree_t roots[] = { comp, spec };

   object_copy_ctx_t *ctx = tree_copy_begin(roots, ARRAY_LEN(roots), NULL, NULL,
                                            NULL, NULL, NULL, NULL);

   collect_generics(comp, ctx);

   tree_copy_finish(roots, ARRAY_LEN(roots), ctx);

   tree_t comp_copy = roots[0], spec_copy = roots[1];

   tree_t b = tree_new(T_BLOCK);
   tree_set_ident(b, dotted);

   map_generics(b, comp_copy, inst);

   tree_copy_ports(b, comp_copy);

   if (!tree_has_value(spec_copy))
      return b;

   // In a recursive instantiation spec_copy may point to a copy of the
   // current architecture so always get the unit from the original spec
   tree_t unit = tree_ref(tree_value(spec));
   tree_t primary = primary_unit_of(unit);

   tree_t sub = tree_new(T_INSTANCE);
   tree_set_loc(sub, tree_loc(spec));
   tree_set_ident(sub, ident_rfrom(tree_ident(primary), '.'));

   tree_t bind = tree_value(spec_copy);

   const int ndecls = tree_decls(spec_copy);
   if (ndecls > 0) {
      tree_t cfg = tree_new(T_CONFIGURATION);
      tree_set_ident(cfg, dotted);
      tree_set_ident2(cfg, tree_ident(bind));
      tree_set_primary(cfg, primary);

      for (int i = 0; i < ndecls; i++)
         tree_add_decl(cfg, tree_decl(spec_copy, i));

      tree_set_ident2(sub, dotted);
      tree_set_class(sub, C_CONFIGURATION);
      tree_set_ref(sub, cfg);
   }
   else {
      tree_set_ident2(sub, tree_ident(bind));
      tree_set_class(sub, C_ENTITY);
      tree_set_ref(sub, unit);
   }

   const int ngenmaps = tree_genmaps(bind);
   for (int i = 0; i < ngenmaps; i++)
      tree_add_genmap(sub, tree_genmap(bind, i));

   const int nparams = tree_params(bind);
   for (int i = 0; i < nparams; i++)
      tree_add_param(sub, tree_param(bind, i));

   tree_add_stmt(b, sub);

   return b;
}

static bool arch_copy_tree_pred(tree_t t, void *ctx)
{
   switch (tree_kind(t)) {
   case T_INSTANCE:
      return true;
   case T_FCALL:
      // Globally static expressions should be copied and folded
      if (tree_frozen(t) && (tree_flags(t) & TREE_F_GLOBALLY_STATIC))
         return true;
      // Fall-through
   case T_PCALL:
      // Call to generic subprogram
      return tree_has_ref(t) && tree_kind(tree_ref(t)) == T_GENERIC_DECL;
   case T_PACKAGE:
   case T_PACK_BODY:
      return true;
   case T_FUNC_DECL:
   case T_PROC_DECL:
      return !(tree_flags(t) & TREE_F_PREDEFINED);
   case T_FUNC_BODY:
   case T_PROC_BODY:
   case T_FUNC_INST:
   case T_PROC_INST:
      return true;
   default:
      return false;
   }
}

tree_t vhdl_architecture_instance(tree_t arch, tree_t bind, ident_t dotted)
{
   assert(tree_kind(arch) == T_ARCH);
   assert(tree_frozen(arch));

   // Architecture must be processed last
   tree_t entity = tree_primary(arch);
   tree_t roots[] = { entity, arch };

   tree_global_flags_t gflags = 0;
   for (int i = 0; i < ARRAY_LEN(roots); i++)
      gflags |= tree_global_flags(roots[i]);

   // The order is important here because the architecture name is
   // prefixed with the entity
   ident_t prefix[] = { tree_ident(arch), tree_ident(tree_primary(arch)) };

   copy_ctx_t copy_ctx = {};

   object_copy_ctx_t *ctx = tree_copy_begin(roots, ARRAY_LEN(roots),
                                            arch_copy_tree_pred, NULL,
                                            NULL, tree_copy_cb, type_copy_cb,
                                            &copy_ctx);

   collect_generics(entity, ctx);

   tree_copy_finish(roots, ARRAY_LEN(roots), ctx);

   rename_mangled(&copy_ctx, dotted, prefix, ARRAY_LEN(prefix));

   tree_t b = tree_new(T_BLOCK);
   tree_set_ident(b, dotted);

   tree_t ent_copy = roots[0], arch_copy = roots[1];

   map_generics(b, ent_copy, bind);

   tree_copy_ports(b, ent_copy);
   tree_copy_decls(b, ent_copy);
   tree_copy_decls(b, arch_copy);
   tree_copy_stmts(b, ent_copy);
   tree_copy_stmts(b, arch_copy);

   tree_set_global_flags(b, gflags);
   return b;
}

static bool config_copy_tree_pred(tree_t t, void *ctx)
{
   return tree_kind(t) == T_INSTANCE;
}

tree_t vhdl_config_instance(tree_t conf, tree_t bind, ident_t dotted)
{
   assert(tree_kind(conf) == T_BLOCK_CONFIG);

   tree_t arch = tree_ref(conf);
   assert(tree_kind(arch) == T_ARCH);

   tree_t roots[] = { tree_primary(arch), arch, conf };

   tree_global_flags_t gflags = 0;
   for (int i = 0; i < ARRAY_LEN(roots); i++)
      gflags |= tree_global_flags(roots[i]);

   object_copy_ctx_t *ctx = tree_copy_begin(roots, ARRAY_LEN(roots),
                                            config_copy_tree_pred,
                                            NULL, NULL, NULL, NULL, NULL);

   tree_copy_finish(roots, ARRAY_LEN(roots), ctx);

   tree_t b = tree_new(T_BLOCK);
   tree_set_ident(b, dotted);

   tree_t ent_copy = roots[0], arch_copy = roots[1], conf_copy = roots[2];

   map_generics(b, ent_copy, bind);

   tree_copy_ports(b, ent_copy);
   tree_copy_decls(b, ent_copy);
   tree_copy_decls(b, conf_copy);
   tree_copy_decls(b, arch_copy);
   tree_copy_stmts(b, ent_copy);
   tree_copy_stmts(b, arch_copy);

   tree_set_global_flags(b, gflags);
   return b;
}

static bool package_copy_tree_pred(tree_t t, void *ctx)
{
   switch (tree_kind(t)) {
   case T_FCALL:
   case T_PCALL:
      // Call to generic subprogram
      return tree_has_ref(t) && tree_kind(tree_ref(t)) == T_GENERIC_DECL;
   case T_FUNC_DECL:
   case T_PROC_DECL:
      return !(tree_flags(t) & TREE_F_PREDEFINED);
   case T_FUNC_BODY:
   case T_PROC_BODY:
   case T_FUNC_INST:
   case T_PROC_INST:
      return true;
   default:
      return false;
   }
}

void vhdl_package_instance(tree_t *roots, int nroots, ident_t dotted,
                           const ident_t *prefixes, int nprefix)
{
   copy_ctx_t copy_ctx = {};

   object_copy_ctx_t *ctx = tree_copy_begin(roots, nroots,
                                            package_copy_tree_pred, NULL,
                                            NULL, tree_copy_cb, type_copy_cb,
                                            &copy_ctx);

   for (int i = 0; i < nroots; i++) {
      switch (tree_kind(roots[i])) {
      case T_PACKAGE:
         collect_generics(roots[i], ctx);
         collect_decls(roots[i], ctx);
         break;
      case T_PACK_BODY:
         collect_decls(roots[i], ctx);
         break;
      default:
         should_not_reach_here();
      }
   }

   tree_copy_finish(roots, nroots, ctx);

   rename_mangled(&copy_ctx, dotted, prefixes, nprefix);
}

void vhdl_subprogram_instance(tree_t *roots, int nroots, ident_t dotted,
                              const ident_t *prefixes, int nprefix)
{
   copy_ctx_t copy_ctx = {};

   object_copy_ctx_t *ctx = tree_copy_begin(roots, nroots,
                                            package_copy_tree_pred, NULL,
                                            NULL, tree_copy_cb, type_copy_cb,
                                            &copy_ctx);

   for (int i = 0; i < nroots; i++) {
      switch (tree_kind(roots[i])) {
      case T_FUNC_DECL:
      case T_PROC_DECL:
         collect_generics(roots[i], ctx);
         break;
      case T_FUNC_BODY:
      case T_PROC_BODY:
         collect_generics(roots[i], ctx);
         collect_decls(roots[i], ctx);
         break;
      default:
         should_not_reach_here();
      }
   }

   tree_copy_finish(roots, nroots, ctx);

   rename_mangled(&copy_ctx, dotted, prefixes, nprefix);
}

static type_t rewrite_generic_types_cb(type_t type, void *__ctx)
{
   hash_t *map = __ctx;
   return hash_get(map, type) ?: type;
}

static tree_t instance_fixup_cb(tree_t t, void *__ctx)
{
   hash_t *map = __ctx;

   switch (tree_kind(t)) {
   case T_REF:
      if (tree_flags(t) & (TREE_F_FORMAL_NAME | TREE_F_ATTR_PREFIX))
         return t;   // Do not rewrite names in generic maps
      // Fall-through
   case T_FCALL:
   case T_PCALL:
   case T_PROT_FCALL:
   case T_PROT_PCALL:
      if (tree_has_ref(t)) {
         tree_t new = hash_get(map, tree_ref(t));
         if (new == NULL)
            return t;
         else if (is_literal(new))
            return new;
         else
            tree_set_ref(t, new);
      }
      break;

   case T_VAR_DECL:
   case T_SIGNAL_DECL:
   case T_CONST_DECL:
      {
         type_t type = tree_type(t);
         if (type_is_unconstrained(type) && !tree_has_value(t))
            error_at(tree_loc(t), "declaration of %s %pI cannot have "
                     "unconstrained type %s", class_str(class_of(t)),
                     tree_ident(t), type_pp(type));
      }
      break;

   default:
      break;
   }

   return t;
}

static void instance_hint_cb(diag_t *d, void *ctx)
{
   tree_t inst = ctx;
   diag_hint(d, tree_loc(inst), "while instantiating %pI", tree_ident(inst));
}

void vhdl_instance_fixup(tree_t inst, hash_t *map)
{
   diag_add_hint_fn(instance_hint_cb, inst);

   tree_rewrite(inst, NULL, instance_fixup_cb, rewrite_generic_types_cb, map);

   diag_remove_hint_fn(instance_hint_cb, inst);
}

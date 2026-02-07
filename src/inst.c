//
//  Copyright (C) 2023-2025  Nick Gasson
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
#include "inst.h"
#include "phase.h"
#include "type.h"
#include "tree.h"

#include <stdlib.h>

typedef A(tree_t) tree_list_t;
typedef A(type_t) type_list_t;

typedef struct {
   tree_list_t copied_subs;
   type_list_t copied_types;
} copy_ctx_t;

static void collect_decls(tree_t t, hset_t *decls, tree_list_t *roots);

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

void copy_with_renaming(tree_t *roots, int nroots, tree_copy_pred_t tree_pred,
                        type_copy_pred_t type_pred, void *context,
                        ident_t dotted, const ident_t *prefixes, int nprefix)
{
   copy_ctx_t copy_ctx = {};

   tree_copy(roots, nroots, tree_pred, type_pred, context,
             tree_copy_cb, type_copy_cb, &copy_ctx);

   // Change the name of any copied types to reflect the new hiearchy
   for (int i = 0; i < copy_ctx.copied_types.count; i++) {
      type_t type = copy_ctx.copied_types.items[i];
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
   ACLEAR(copy_ctx.copied_types);

   // Change the mangled name of copied subprograms so that copies in
   // different instances do not collide
   for (int i = 0; i < copy_ctx.copied_subs.count; i++) {
      tree_t decl = copy_ctx.copied_subs.items[i];
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

            if (tree_flags(decl) & TREE_F_PREDEFINED)
               tb_cat(tb, "$predef");

            ident_t mangled = ident_new(tb_get(tb));
            tree_set_ident2(decl, mangled);

            break;
         }
      }
   }
   ACLEAR(copy_ctx.copied_subs);
}

static bool instantiate_should_copy_type(type_t type, void *__ctx)
{
   hset_t *decls = __ctx;
   if (decls == NULL || type_kind(type) == T_SUBTYPE)
      return false;
   else
      return hset_contains(decls, type);
}

static bool instantiate_should_copy_tree(tree_t t, void *__ctx)
{
   hset_t *decls = __ctx;

   switch (tree_kind(t)) {
   case T_INSTANCE:
      return true;
   case T_FCALL:
      // Globally static expressions should be copied and folded
      if ((tree_flags(t) & TREE_F_GLOBALLY_STATIC)
          && type_is_scalar(tree_type(t)))
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
   case T_CONST_DECL:
   case T_VAR_DECL:
   case T_SIGNAL_DECL:
   case T_GENERIC_DECL:
      return hset_contains(decls, t);
   default:
      return false;
   }
}

static void collect_generic_types(hset_t *decls, type_t type)
{
   hset_insert(decls, type);

   // Also collect any anonymous generic types
   switch (type_subkind(type)) {
   case GTYPE_ARRAY:
      {
         type_t elem = type_elem(type);
         if (type_kind(elem) == T_GENERIC && !type_has_ident(elem))
            collect_generic_types(decls, elem);

         const int nindex = type_indexes(type);
         for (int i = 0; i < nindex; i++) {
            type_t index = type_index(type, i);
            if (type_kind(index) == T_GENERIC && !type_has_ident(index))
               collect_generic_types(decls, index);
         }
      }
      break;

   default:
      break;
   }
}

static void collect_generics(tree_t t, hset_t *decls, tree_list_t *roots)
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
               collect_generics(pack, decls, roots);
               collect_decls(pack, decls, roots);
               APUSH(*roots, pack);
            }
         }
         break;
      case C_TYPE:
         collect_generic_types(decls, tree_type(g));
         break;
      case C_CONSTANT:
         hset_insert(decls, g);
         break;
      default:
         break;
      }
   }
}

static void collect_decls(tree_t t, hset_t *decls, tree_list_t *roots)
{
   const int ndecls = tree_decls(t);
   for (int i = 0 ; i < ndecls; i++) {
      tree_t d = tree_decl(t, i);
      hset_insert(decls, d);

      switch (tree_kind(d)) {
      case T_PACKAGE:
         collect_generics(d, decls, roots);
         // Fall-through
      case T_PACK_BODY:
      case T_PACK_INST:
         collect_decls(d, decls, roots);
         break;
      case T_TYPE_DECL:
         hset_insert(decls, type_base_recur(tree_type(d)));
         break;
      case T_PROT_DECL:
      case T_PROT_BODY:
         hset_insert(decls, tree_type(d));
         collect_decls(d, decls, roots);
         break;
      default:
         break;
      }
   }
}

void new_instance(tree_t *roots, int nroots, ident_t dotted,
                  const ident_t *prefixes, int nprefix)
{
   hset_t *decls = hset_new(64);
   tree_list_t troots = AINIT;

   for (int i = 0; i < nroots; i++)
      APUSH(troots, roots[i]);

   for (int i = 0; i < nroots; i++) {
      switch (tree_kind(roots[i])) {
      case T_ENTITY:
      case T_FUNC_DECL:
      case T_PROC_DECL:
      case T_COMPONENT:
         collect_generics(roots[i], decls, &troots);
         break;
      case T_PACKAGE:
      case T_FUNC_BODY:
      case T_PROC_BODY:
      case T_FUNC_INST:
      case T_PROC_INST:
         collect_generics(roots[i], decls, &troots);
         collect_decls(roots[i], decls, &troots);
         break;
      case T_PACK_BODY:
         collect_decls(roots[i], decls, &troots);
         break;
      default:
         break;
      }
   }

   copy_with_renaming(troots.items, troots.count,
                      instantiate_should_copy_tree,
                      instantiate_should_copy_type,
                      decls, dotted, prefixes, nprefix);

   for (int i = 0; i < nroots; i++)
      roots[i] = troots.items[i];

   ACLEAR(troots);
   hset_free(decls);
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
            error_at(tree_loc(t), "declaration of %s %s cannot have "
                     "unconstrained type %s", class_str(class_of(t)),
                     istr(tree_ident(t)), type_pp(type));
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
   diag_hint(d, tree_loc(inst), "while instantiating %s",
             istr(tree_ident(inst)));
}

void instance_fixup(tree_t inst, hash_t *map)
{
   diag_add_hint_fn(instance_hint_cb, inst);

   tree_rewrite(inst, NULL, instance_fixup_cb, rewrite_generic_types_cb, map);

   diag_remove_hint_fn(instance_hint_cb);
}

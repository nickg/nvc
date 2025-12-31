//
//  Copyright (C) 2024-2025  Nick Gasson
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
#include "ident.h"
#include "mask.h"
#include "mir/mir-node.h"
#include "mir/mir-priv.h"
#include "mir/mir-structs.h"
#include "option.h"
#include "printf.h"

#include <assert.h>
#include <stdlib.h>
#include <string.h>

#define CFG_EMBED_EDGES 4

typedef struct {
   unsigned count;
   unsigned max;
   union {
      mir_block_t  edges[CFG_EMBED_EDGES];
      mir_block_t *external;
   } u;
} cfg_edge_list_t;

typedef struct {
   mir_block_t     block;
   unsigned        entry : 1;
   unsigned        aborts : 1;
   unsigned        returns : 1;
   cfg_edge_list_t in;
   cfg_edge_list_t out;
   bit_mask_t      dom;
   bit_mask_t      livein;
   bit_mask_t      varkill;
   bit_mask_t      liveout;
} cfg_block_t;

typedef uint32_t valnum_t;
#define VN_INVALID UINT_MAX
#define VN_CONST   0x80000000

typedef struct {
   mir_value_t value;
   uint32_t    hash;
   mir_block_t block;
   valnum_t    vn;
} gvn_tab_t;

typedef struct {
   valnum_t    *nodevn;
   valnum_t    *paramvn;
   valnum_t     nextvn;
   valnum_t     maxvn;
   mir_value_t *canon;
   bit_mask_t   visited;
   size_t       tabsz;
   gvn_tab_t    hashtab[];
} gvn_state_t;

typedef struct {
   cfg_block_t *cfg;
   gvn_state_t *gvn;
} mir_optim_t;

static void mir_dump_optim(mir_unit_t *mu, mir_optim_t *an);

////////////////////////////////////////////////////////////////////////////////
// Control flow graph construction

static void cfg_add_one_edge(cfg_edge_list_t *list, mir_block_t edge)
{
   if (list->count < 4)
      list->u.edges[list->count++] = edge;
   else if (list->count == 4) {
      mir_block_t *ptr = xmalloc_array(16, sizeof(mir_block_t));
      memcpy(ptr, list->u.edges, 4 * sizeof(mir_block_t));

      list->max = 16;
      list->u.external = ptr;
      list->u.external[list->count++] = edge;
   }
   else if (list->count == list->max) {
      list->max *= 2;
      list->u.external =
         xrealloc_array(list->u.external, list->max, sizeof(mir_block_t));
      list->u.external[list->count++] = edge;
   }
   else
      list->u.external[list->count++] = edge;
}

static void cfg_add_edge(cfg_block_t *cfg, mir_block_t from, mir_block_t to)
{
   cfg_add_one_edge(&(cfg[from.id].out), to);
   cfg_add_one_edge(&(cfg[to.id].in), from);
}

static mir_block_t cfg_get_edge(const cfg_edge_list_t *list, int nth)
{
   assert(nth < list->count);
   if (list->max <= 4)
      return list->u.edges[nth];
   else
      return list->u.external[nth];
}

static cfg_block_t *mir_get_cfg(mir_unit_t *mu)
{
   cfg_block_t *cfg = xcalloc_array(mu->blocks.count, sizeof(cfg_block_t));
   cfg[0].entry = true;

   if (mir_get_kind(mu) == MIR_UNIT_PROCESS && mu->blocks.count > 1)
      cfg[1].entry = true;

   for (int i = 0; i < mu->blocks.count; i++) {
      const block_data_t *bd = &(mu->blocks.items[i]);

      mir_block_t this = { .tag = MIR_TAG_BLOCK, .id = i };
      cfg[i].block = this;

      DEBUG_ONLY(mir_set_cursor(mu, this, MIR_APPEND));
      MIR_ASSERT(bd->num_nodes > 0, "empty basic block");

      const node_data_t *last = &(mu->nodes[bd->nodes[bd->num_nodes - 1]]);
      MIR_ASSERT(mir_is_terminator(last->op),
                 "last operation in block is not terminator");

      switch (last->op) {
      case MIR_OP_WAIT:
         assert(last->args[0].tag == MIR_TAG_BLOCK);
         cfg[last->args[0].id].entry = true;
         // Fall-through
      case MIR_OP_RETURN:
         cfg[i].returns = true;
         break;
      default:
         {
            const mir_value_t *args = mir_get_args(mu, last);
            for (int j = 0; j < last->nargs; j++) {
               if (args[j].tag == MIR_TAG_BLOCK)
                  cfg_add_edge(cfg, this, mir_cast_block(args[j]));
            }
         }
         break;
      }
   }

   return cfg;
}

static void mir_free_cfg(mir_unit_t *mu, cfg_block_t *cfg)
{
   for (int i = 0; i < mu->blocks.count; i++) {
      cfg_block_t *cb = &(cfg[i]);
      if (cb->in.max > CFG_EMBED_EDGES) free(cb->in.u.external);
      if (cb->out.max > CFG_EMBED_EDGES) free(cb->out.u.external);

      mask_free(&cb->dom);
      mask_free(&cb->livein);
      mask_free(&cb->varkill);
      mask_free(&cb->liveout);
   }

   free(cfg);
}

////////////////////////////////////////////////////////////////////////////////
// Dominator tree construction

static void mir_dominator_tree(mir_unit_t *mu, cfg_block_t *cfg)
{
   // TODO: use more efficient algorithm

   for (int i = 0; i < mu->blocks.count; i++) {
      mask_init(&cfg[i].dom, mu->blocks.count);

      if (cfg[i].entry)
         mask_set(&cfg[i].dom, i);
      else
         mask_setall(&cfg[i].dom);
   }

   bit_mask_t tmp;
   mask_init(&tmp, mu->blocks.count);

   bool changes;
   do {
      changes = false;

      for (int i = 0; i < mu->blocks.count; i++) {
         if (cfg[i].entry) continue;

         mask_setall(&tmp);

         for (int j = 0; j < cfg[i].in.count; j++) {
            mir_block_t pred = cfg_get_edge(&cfg[i].in, j);
            mask_intersect(&tmp, &cfg[pred.id].dom);
         }

         mask_set(&tmp, i);

         if (!changes && !mask_eq(&tmp, &cfg[i].dom))
            changes = true;

         mask_copy(&cfg[i].dom, &tmp);
      }
   } while (changes);
}

////////////////////////////////////////////////////////////////////////////////
// Liveness analysis

static void mir_do_liveness(mir_unit_t *mu, mir_optim_t *opt)
{
   // Algorithm from "Engineering a Compiler" chapter 8.6

   for (int i = 0; i < mu->blocks.count; i++) {
      cfg_block_t *cb = &(opt->cfg[i]);
      mask_init(&cb->livein, mu->num_nodes);
      mask_init(&cb->varkill, mu->num_nodes);
      mask_init(&cb->liveout, mu->num_nodes);

      const block_data_t *bd = mir_block_data(mu, cb->block);
      for (int j = 0; j < bd->num_nodes; j++) {
         mir_value_t node = { .tag = MIR_TAG_NODE, .id = bd->nodes[j] };
         const node_data_t *n = mir_node_data(mu, node);

         mask_set(&cb->varkill, node.id);

         if (n->op == MIR_OP_CONST || n->op == MIR_OP_CONST_REAL)
            continue;   // Arguments have special encoding

         const mir_value_t *args = mir_get_args(mu, n);
         for (int k = 0; k < n->nargs; k++) {
            mir_value_t arg = args[k];
            if (arg.tag == MIR_TAG_NODE && !mask_test(&cb->varkill, arg.id))
               mask_set(&cb->livein, arg.id);
         }
      }
   }

   bit_mask_t new, tmp;
   mask_init(&new, mu->num_nodes);
   mask_init(&tmp, mu->num_nodes);

   bool changed;
   do {
      changed = false;

      for (int i = mu->blocks.count - 1; i >= 0; i--) {
         cfg_block_t *cb = &(opt->cfg[i]);
         mask_clearall(&new);

         for (int j = 0; j < cb->out.count; j++) {
            cfg_block_t *succ = &(opt->cfg[cfg_get_edge(&cb->out, j).id]);
            mask_copy(&tmp, &succ->liveout);
            mask_subtract(&tmp, &succ->varkill);
            mask_union(&tmp, &succ->livein);
            mask_union(&new, &tmp);
         }

         if (!mask_eq(&new, &cb->liveout)) {
            mask_copy(&cb->liveout, &new);
            changed = true;
         }
      }
   } while (changed);

   // Replaced "upward exposed variables" set with live-in
   for (int i = 0; i < mu->blocks.count; i++) {
      cfg_block_t *cb = &(opt->cfg[i]);
      mask_copy(&tmp, &cb->liveout);
      mask_subtract(&tmp, &cb->varkill);
      mask_union(&cb->livein, &tmp);
   }

   mask_free(&new);
   mask_free(&tmp);
}

////////////////////////////////////////////////////////////////////////////////
// Global value numbering

static inline valnum_t gvn_new_value(mir_value_t canon, gvn_state_t *gvn)
{
   assert(gvn->nextvn < gvn->maxvn);
   gvn->canon[gvn->nextvn] = canon;
   return gvn->nextvn++;
}

static inline bool gvn_tracked(mir_value_t value)
{
   return value.tag == MIR_TAG_NODE || value.tag == MIR_TAG_PARAM;
}

static valnum_t gvn_get_value(mir_value_t value, gvn_state_t *gvn)
{
   switch (value.tag) {
   case MIR_TAG_NODE:
      return gvn->nodevn[value.id];
   case MIR_TAG_PARAM:
      return gvn->paramvn[value.id];
   case MIR_TAG_CONST:
      return VN_CONST + value.id;
   default:
      should_not_reach_here();
   }
}

static uint32_t gvn_hash_node(mir_unit_t *mu, const node_data_t *n,
                              gvn_state_t *gvn)
{
   uint32_t hash = knuth_hash(n->op) + knuth_hash(n->nargs);
   hash ^= mir_type_data(mu, n->type)->hash;

   if (n->op == MIR_OP_CONST)
      return hash ^ mix_bits_64(n->iconst);
   else if (n->op == MIR_OP_CONST_REAL)
      return hash ^ mix_bits_64(FLOAT_BITS(n->dconst));
   else if (n->op == MIR_OP_CONST_VEC)
      return hash ^ mix_bits_64(n->bits[0]) ^ mix_bits_64(n->bits[1]);
   else if (n->op == MIR_OP_LOCUS)
      return hash ^ mix_bits_64(n->locus);
   else {
      const mir_value_t *args = mir_get_args(mu, n);
      for (int i = 0; i < n->nargs; i++) {
         hash <<= 1;   // Argument order should affect hash
         if (gvn_tracked(args[i]))
            hash += knuth_hash(gvn_get_value(args[i], gvn));
         else
            hash += knuth_hash(args[i].bits);
      }

      return mix_bits_32(hash);
   }
}

static bool gvn_compare(mir_unit_t *mu, const node_data_t *a,
                        const node_data_t *b, gvn_state_t *gvn)
{
   if (a->op != b->op || a->nargs != b->nargs || !mir_equals(a->type, b->type))
      return false;
   else if (a->op == MIR_OP_CONST)
      return a->iconst == b->iconst;
   else if (a->op == MIR_OP_CONST_REAL)
      return a->dconst == b->dconst;
   else if (a->op == MIR_OP_CONST_VEC)
      return a->bits[0] == b->bits[0] && a->bits[1] == b->bits[1];
   else if (a->op == MIR_OP_LOCUS)
      return a->locus == b->locus;

   const mir_value_t *a_args = mir_get_args(mu, a);
   const mir_value_t *b_args = mir_get_args(mu, b);

   for (int i = 0; i < a->nargs; i++) {
      if (a_args[i].tag != b_args[i].tag)
         return false;
      else if (gvn_tracked(a_args[i])) {
         const valnum_t avn = gvn_get_value(a_args[i], gvn);
         const valnum_t bvn = gvn_get_value(b_args[i], gvn);
         if (avn != bvn)
            return false;
      }
      else if (a_args[i].bits != b_args[i].bits)
         return false;
   }

   return true;
}

static void gvn_generic(mir_unit_t *mu, mir_value_t node, mir_block_t block,
                        mir_optim_t *opt)
{
   const node_data_t *n = mir_node_data(mu, node);
   const cfg_block_t *cb = &(opt->cfg[block.id]);
   const uint32_t hash = gvn_hash_node(mu, n, opt->gvn);

   for (int idx = hash & (opt->gvn->tabsz - 1), limit = 0; limit < 10;
        idx = (idx + 1) & (opt->gvn->tabsz - 1), limit++) {
      gvn_tab_t *tab = &(opt->gvn->hashtab[idx]);
      if (mir_is_null(tab->value)) {
         tab->value = node;
         tab->hash  = hash;
         tab->block = block;
         tab->vn = opt->gvn->nodevn[node.id] = gvn_new_value(node, opt->gvn);
         return;
      }
      else if (tab->hash == hash && mask_test(&cb->dom, tab->block.id)) {
         const node_data_t *o = mir_node_data(mu, tab->value);
         if (gvn_compare(mu, n, o, opt->gvn)) {
            opt->gvn->nodevn[node.id] = tab->vn;
            return;
         }
      }
   }

   opt->gvn->nodevn[node.id] = gvn_new_value(node, opt->gvn);
}

static void gvn_sub(mir_unit_t *mu, mir_value_t node, mir_block_t block,
                    mir_optim_t *opt)
{
   node_data_t *n = mir_node_data(mu, node);
   assert(n->nargs == 2);

   // (X + Y) - Y ==> X

   mir_value_t left = n->args[0], right = n->args[1];
   if (left.tag == MIR_TAG_NODE) {
      node_data_t *nl = mir_node_data(mu, left);
      if (nl->op == MIR_OP_ADD) {
         valnum_t left0_vn = gvn_get_value(nl->args[0], opt->gvn);
         valnum_t left1_vn = gvn_get_value(nl->args[1], opt->gvn);
         valnum_t right_vn = gvn_get_value(right, opt->gvn);
         if (left1_vn == right_vn) {
            opt->gvn->nodevn[node.id] = left0_vn;
            return;
         }
      }
   }

   gvn_generic(mu, node, block, opt);
}

static void gvn_commutative(mir_unit_t *mu, mir_value_t node, mir_block_t block,
                            mir_optim_t *opt, int first)
{
   node_data_t *n = mir_node_data(mu, node);
   assert(n->nargs == first + 2);
   assert(n->nargs <= MIR_INLINE_ARGS);

   mir_value_t left = n->args[first], right = n->args[first + 1];
   if (left.tag == MIR_TAG_CONST) {
      // Move constants to right hand side
      n->args[first] = right;
      n->args[first + 1] = left;
   }
   else if (gvn_tracked(left) && gvn_tracked(right)) {
      // Sort arguments by value number
      const valnum_t vn0 = gvn_get_value(left, opt->gvn);
      const valnum_t vn1 = gvn_get_value(right, opt->gvn);

      if (vn0 > vn1) {
         n->args[first] = right;
         n->args[first + 1] = left;
      }
   }

   gvn_generic(mu, node, block, opt);
}

static void gvn_cmp(mir_unit_t *mu, mir_value_t node, mir_block_t block,
                    mir_optim_t *opt)
{
   node_data_t *n = mir_node_data(mu, node);
   assert(n->nargs == 3);
   assert(n->nargs <= MIR_INLINE_ARGS);
   assert(n->args[0].tag == MIR_TAG_ENUM);

   switch (n->args[0].id) {
   case MIR_CMP_EQ:
   case MIR_CMP_NEQ:
      gvn_commutative(mu, node, block, opt, 1);
      break;
   default:
      gvn_generic(mu, node, block, opt);
      break;
   }
}

static void gvn_logical(mir_unit_t *mu, mir_value_t node, mir_block_t block,
                        mir_optim_t *opt)
{
   const node_data_t *n = mir_node_data(mu, node);
   assert(n->nargs == 2);
   assert(n->nargs <= MIR_INLINE_ARGS);

   if (gvn_tracked(n->args[0]) && gvn_tracked(n->args[1])) {
      const valnum_t vn0 = gvn_get_value(n->args[0], opt->gvn);
      const valnum_t vn1 = gvn_get_value(n->args[1], opt->gvn);
      if (vn0 == vn1) {
         switch (n->op) {
         case MIR_OP_AND:   // X & X ==> X
         case MIR_OP_OR:    // X | X ==> X
            opt->gvn->nodevn[node.id] = vn0;
            return;
         default:
            should_not_reach_here();
         }
      }
   }

   gvn_commutative(mu, node, block, opt, 0);
}

static void gvn_phi(mir_unit_t *mu, mir_value_t node, mir_block_t block,
                    mir_optim_t *opt)
{
   const node_data_t *n = mir_node_data(mu, node);
   assert(n->nargs >= 2);

   const mir_value_t *args = mir_get_args(mu, n);

   if (!gvn_tracked(args[1]))
      return;

   valnum_t vn = gvn_get_value(args[1], opt->gvn);

   for (int i = 3; i < n->nargs; i += 2) {
      if (gvn_get_value(args[i], opt->gvn) != vn)
         return;
   }

   opt->gvn->nodevn[node.id] = vn;
}

static void gvn_unpack(mir_unit_t *mu, mir_value_t node, mir_block_t block,
                       mir_optim_t *opt)
{
   const node_data_t *n = mir_node_data(mu, node);
   if (mir_is_null(n->type))
      return;   // Unpack into memory

   gvn_generic(mu, node, block, opt);
}

static void gvn_load(mir_unit_t *mu, mir_value_t node, mir_block_t block,
                     mir_optim_t *opt)
{
   // TODO: check if stamp is const or pointer

   opt->gvn->nodevn[node.id] = gvn_new_value(node, opt->gvn);
}

static void gvn_visit_block(mir_unit_t *mu, mir_block_t block,
                            mir_optim_t *opt)
{
   if (mask_test(&opt->gvn->visited, block.id))
      return;

   mask_set(&opt->gvn->visited, block.id);

   for (size_t id = -1; mask_iter(&opt->cfg[block.id].dom, &id);) {
      mir_block_t dom = { .tag = MIR_TAG_BLOCK, .id = id };
      gvn_visit_block(mu, dom, opt);
   }

   const block_data_t *bd = mir_block_data(mu, block);
   for (int i = 0; i < bd->num_nodes; i++) {
      mir_value_t node = { .tag = MIR_TAG_NODE, .id = bd->nodes[i] };
      switch (mir_get_op(mu, node)) {
      case MIR_OP_CMP:
         gvn_cmp(mu, node, block, opt);
         break;
      case MIR_OP_ADD:
      case MIR_OP_MUL:
         gvn_commutative(mu, node, block, opt, 0);
         break;
      case MIR_OP_SUB:
         gvn_sub(mu, node, block, opt);
         break;
      case MIR_OP_NOT:
      case MIR_OP_RESOLVED:
      case MIR_OP_SELECT:
      case MIR_OP_REM:
      case MIR_OP_LINK_PACKAGE:
      case MIR_OP_LINK_VAR:
      case MIR_OP_PACKAGE_INIT:
      case MIR_OP_LOCUS:
      case MIR_OP_UARRAY_LEN:
      case MIR_OP_RANGE_LENGTH:
      case MIR_OP_CAST:
      case MIR_OP_PACK:
      case MIR_OP_VAR_UPREF:
      case MIR_OP_CONST:
      case MIR_OP_CONST_REAL:
      case MIR_OP_CONST_VEC:
      case MIR_OP_BINARY:
      case MIR_OP_UNARY:
      case MIR_OP_CLOSURE:
      case MIR_OP_CONTEXT_UPREF:
      case MIR_OP_UNWRAP:
      case MIR_OP_INSERT:
      case MIR_OP_NULL:
         gvn_generic(mu, node, block, opt);
         break;
      case MIR_OP_AND:
      case MIR_OP_OR:
         gvn_logical(mu, node, block, opt);
         break;
      case MIR_OP_PHI:
         gvn_phi(mu, node, block, opt);
         break;
      case MIR_OP_UNPACK:
         gvn_unpack(mu, node, block, opt);
         break;
      case MIR_OP_LOAD:
         gvn_load(mu, node, block, opt);
         break;
      case MIR_OP_INIT_SIGNAL:
      case MIR_OP_PORT_CONVERSION:
      case MIR_OP_PROTECTED_INIT:
         opt->gvn->nodevn[node.id] = gvn_new_value(node, opt->gvn);
         break;
      default:
         break;
      }
   }
}

static void mir_do_gvn(mir_unit_t *mu, mir_optim_t *opt)
{
   const size_t tabsz = next_power_of_2(mu->num_nodes * 2);
   valnum_t *vnmap = xmalloc_array(mu->num_nodes + mu->params.count,
                                   sizeof(gvn_tab_t));

   opt->gvn = xcalloc_flex(sizeof(gvn_state_t), tabsz, sizeof(gvn_tab_t));
   opt->gvn->tabsz   = tabsz;
   opt->gvn->nodevn  = vnmap;
   opt->gvn->paramvn = vnmap + mu->num_nodes;
   opt->gvn->maxvn   = mu->num_nodes + mu->params.count;
   opt->gvn->canon   = xcalloc_array(opt->gvn->maxvn, sizeof(mir_value_t));

   for (int i = 0; i < mu->num_nodes; i++)
      opt->gvn->nodevn[i] = VN_INVALID;

   for (int i = 0; i < mu->params.count; i++) {
      mir_value_t param = { .tag = MIR_TAG_PARAM, .id = i };
      opt->gvn->paramvn[i] = gvn_new_value(param, opt->gvn);
   }

   mask_init(&opt->gvn->visited, mu->blocks.count);

   for (int i = 0; i < mu->blocks.count; i++) {
      mir_block_t this = { .tag = MIR_TAG_BLOCK, .id = i };
      gvn_visit_block(mu, this, opt);
   }

   for (int i = 0; i < mu->num_nodes; i++) {
      node_data_t *n = &(mu->nodes[i]);
      const mir_value_t *args = mir_get_args(mu, n);
      for (int j = 0; j < n->nargs; j++) {
         if (args[j].tag == MIR_TAG_NODE) {
            const valnum_t vn = opt->gvn->nodevn[args[j].id];
            if (unlikely(vn == VN_INVALID)) {
               mir_dump_optim(mu, opt);
               fatal_trace("node %%%d has no value number", args[j].id);
            }
            else if (!mir_equals(opt->gvn->canon[vn], args[j]))
               mir_set_arg(mu, n, j, opt->gvn->canon[vn]);
         }
      }
   }

   if (opt_get_verbose(OPT_GVN_VERBOSE, istr(mu->name)))
      mir_dump_optim(mu, opt);

   mask_free(&opt->gvn->visited);
   free(vnmap);
   free(opt->gvn->canon);
   free(opt->gvn);
   opt->gvn = NULL;
}

////////////////////////////////////////////////////////////////////////////////
// Dead code elimination using liveness information

static void mir_do_dce(mir_unit_t *mu, mir_optim_t *opt)
{
   bit_mask_t live;
   mask_init(&live, mu->num_nodes);

   for (int i = mu->blocks.count - 1; i >= 0; i--) {
      mir_block_t this = { .tag = MIR_TAG_BLOCK, .id = i };
      const block_data_t *bd = mir_block_data(mu, this);

      mask_copy(&live, &(opt->cfg[i].liveout));

      for (int j = bd->num_nodes - 1; j >= 0; j--) {
         mir_value_t node = { .tag = MIR_TAG_NODE, .id = bd->nodes[j] };
         node_data_t *n = mir_node_data(mu, node);

         if (!mir_is_null(n->type) && !mask_test(&live, node.id)) {
            DEBUG_ONLY(const mir_op_t op = n->op);

            mir_set_cursor(mu, this, j);
            mir_delete(mu);

            DEBUG_ONLY(mir_comment(mu, "Dead %s definition of %%%u",
                                   mir_op_string(op), node.id));
         }

         mask_clear(&live, node.id);

         const mir_value_t *args = mir_get_args(mu, n);

         for (int k = 0; k < n->nargs; k++) {
            mir_value_t arg = args[k];
            if (arg.tag == MIR_TAG_NODE)
               mask_set(&live, arg.id);
         }
      }
   }

   if (opt_get_verbose(OPT_DCE_VERBOSE, istr(mu->name)))
      mir_dump_optim(mu, opt);

   mir_compact(mu);

   mask_free(&live);
}

////////////////////////////////////////////////////////////////////////////////
// Debugging

// LCOV_EXCL_START /////////////////////////////////////////////////////////////

static void dump_node_set(const bit_mask_t *mask, const char *tag)
{
   if (mask_popcount(mask) == 0)
      return;

   printf(" %s:{", tag);
   for (size_t bit = -1, nth = 0; mask_iter(mask, &bit); nth++)
      printf("%s%%%zd", nth++ > 0 ? "," : "", bit);
   printf("}");
}

static int begin_block_cb(mir_unit_t *mu, mir_block_t block, int col, void *ctx)
{
   const mir_optim_t *opt = ctx;

   if (opt->cfg == NULL)
      return 0;

   const cfg_block_t *cb = &(opt->cfg[block.id]);
   nvc_printf("$cyan$//");

   if (cb->entry) printf(" entry");

   if (cb->in.count > 0) {
      printf(" in:");
      for (int i = 0; i < cb->in.count; i++)
         printf("%s%d", i > 0 ? "," : "", cfg_get_edge(&cb->in, i).id);

   }
   else if (!cb->entry)
      printf(" unreachable");

   if (cb->out.count > 0) {
      printf(" out:");
      for (int i = 0; i < cb->out.count; i++)
         printf("%s%d", i > 0 ? "," : "", cfg_get_edge(&cb->out, i).id);
   }

   if (cb->dom.size > 0) {
      printf(" dom:");

      for (size_t bit = -1, nth = 0; mask_iter(&(cb->dom), &bit); nth++)
         printf("%s%zd", nth > 0 ? "," : "", bit);
   }

   if (cb->livein.size > 0) {
      dump_node_set(&cb->livein, "livein");
      dump_node_set(&cb->liveout, "liveout");
   }

   if (cb->returns) printf(" returns");
   if (cb->aborts) printf(" aborts");

   nvc_printf("$$\n%*.s", col, "");

   return 0;
}

static int value_cb(mir_unit_t *mu, mir_value_t value, void *ctx)
{
   const mir_optim_t *opt = ctx;

   if (opt->gvn != NULL && gvn_tracked(value)) {
      int col = nvc_printf("$!black$(");

      if (value.tag == MIR_TAG_NODE) {
         const node_data_t *n = mir_node_data(mu, value);
         col += printf("%08x:", gvn_hash_node(mu, n, opt->gvn));
      }

      if (opt->gvn->nodevn[value.id] == VN_INVALID)
         col += printf("-");
      else
         col += printf("%u", gvn_get_value(value, opt->gvn));

      col += nvc_printf(")$$");
      return col;
   }
   else
      return 0;
}

static void mir_dump_optim(mir_unit_t *mu, mir_optim_t *an)
{
   const mir_annotate_t cb = {
      .begin_block = begin_block_cb,
      .value = value_cb,
   };

   mir_annotate(mu, &cb, an);
}

// LCOV_EXCL_STOP //////////////////////////////////////////////////////////////

////////////////////////////////////////////////////////////////////////////////
// Public interface

void mir_optimise(mir_unit_t *mu, mir_pass_t passes)
{
   mir_optim_t opt = {};

   const mir_pass_t need_dom = MIR_PASS_GVN;
   const mir_pass_t need_liveness = MIR_PASS_DCE;
   const mir_pass_t need_cfg = need_dom | need_liveness;

   if (passes & need_cfg)
      opt.cfg = mir_get_cfg(mu);

   if (passes & need_dom)
      mir_dominator_tree(mu, opt.cfg);

   if (passes & MIR_PASS_GVN)
      mir_do_gvn(mu, &opt);

   if (passes & need_liveness)
      mir_do_liveness(mu, &opt);

   if (passes & MIR_PASS_DCE)
      mir_do_dce(mu, &opt);

   if (passes & need_cfg)
      mir_free_cfg(mu, opt.cfg);
}

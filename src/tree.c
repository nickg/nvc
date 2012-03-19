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

#include "tree.h"
#include "util.h"

#include <assert.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

#define MAX_CONTEXTS 16
#define MAX_ATTRS    16
#define FILE_FMT_VER 0x1006

//#define EXTRA_READ_CHECKS

struct tree_array {
   size_t count;
   size_t max;
   tree_t *items;
};

struct param_array {
   size_t  count;
   size_t  max;
   param_t *items;
};

typedef enum {
   A_STRING, A_INT, A_PTR, A_TREE
} attr_kind_t;

struct attr {
   attr_kind_t kind;
   ident_t     name;
   union {
      ident_t sval;
      int     ival;
      void    *pval;
      tree_t  tval;
   };
};

struct tree {
   tree_kind_t kind;
   loc_t       loc;
   ident_t     ident;
   struct attr *attrs;
   unsigned    n_attrs;

   union {
      struct tree_array  decls;    // T_ARCH, T_PROCESS, T_PACKAGE, T_FUNC_BODY
   };
   union {
      struct tree_array  ports;    // T_ENTITY, T_FUNC_DECL, T_FUNC_BODY
      struct param_array params;   // T_FCALL, T_ATTR_REF, T_PCALL
      struct tree_array  drivers;  // T_SIGNAL_DECL
   };
   union {
      struct tree_array  generics; // T_ENTITY
      struct tree_array  stmts;    // T_ARCH, T_PROCESS, T_PACKAGE, T_FUNC_BODY
      struct tree_array  waves;    // T_SIGNAL_ASSIGN, T_COND
      struct param_array genmaps;  // T_INSTANCE
      struct tree_array  conds;    // T_CASSIGN
   };
   union {
      literal_t   literal;         // T_LITERAL
      port_mode_t port_mode;       // T_PORT_MODE
      ident_t     ident2;          // T_ARCH, T_ATTR_REF
      tree_t      message;         // T_ASSERT
      tree_t      delay;           // T_WAIT
      tree_t      reject;          // T_CASSIGN, T_SIGNAL_ASSIGN
   };
   union {
      tree_t   target;             // T_VAR_ASSIGN, T_SIGNAL_ASSIGN
      tree_t   ref;                // T_REF, T_FCALL, T_ARRAY_REF, T_PCALL
      tree_t   severity;           // T_ASSERT
      unsigned pos;                // T_ENUM_LIT;
   };
   union {
      struct {                     // T_AGGREGATE
         assoc_t  *assocs;
         unsigned n_assocs;
         unsigned n_assocs_alloc;
      };
      struct {                     // T_ARCH, T_ENTITY, T_PACKAGE
         context_t *context;
         unsigned  n_contexts;
      };
      struct {                     // T_SIGNAL_DECL
         struct tree_array *sub_drivers;
         unsigned          n_elems;
      };
      range_t           range;     // T_ARRAY_SLICE
      struct tree_array triggers;  // T_WAIT, T_PROCESS
      struct tree_array elses;     // T_IF
      class_t           class;     // T_PORT_DECL
   };
   type_t type;                    // many
   tree_t value;                   // many

   // Serialisation and GC bookkeeping
   uint32_t generation;
   uint32_t index;
};

struct tree_wr_ctx {
   FILE           *file;
   type_wr_ctx_t  type_ctx;
   ident_wr_ctx_t ident_ctx;
   unsigned       generation;
   unsigned       n_trees;
   const char     *file_names[256];
};

struct tree_rd_ctx {
   FILE           *file;
   type_rd_ctx_t  type_ctx;
   ident_rd_ctx_t ident_ctx;
   unsigned       n_trees;
   tree_t         *store;
   unsigned       store_sz;
   char           *db_fname;
   const char     *file_names[256];
};

#define IS(t, k) ((t)->kind == (k))
#define IS_TOP_LEVEL(t)                                               \
   (IS(t, T_ARCH) || IS(t, T_ENTITY) || IS(t, T_PACKAGE)              \
    || IS(t, T_ELAB) || IS(t, T_PACK_BODY))
#define IS_DECL(t)                                                    \
   (IS(t, T_PORT_DECL) || IS(t, T_SIGNAL_DECL) || IS(t, T_VAR_DECL)   \
    || IS(t, T_TYPE_DECL) || IS(t, T_CONST_DECL)                      \
    || IS(t, T_FUNC_DECL) || IS(t, T_FUNC_BODY) || IS(t, T_ALIAS)     \
    || IS(t, T_ATTR_DECL) || IS(t, T_ATTR_SPEC) || IS(t, T_PROC_DECL) \
    || IS(t, T_PROC_BODY))
#define IS_EXPR(t)                                                    \
   (IS(t, T_FCALL) || IS(t, T_LITERAL) || IS(t, T_REF)                \
    || IS(t, T_QUALIFIED) || IS(t, T_AGGREGATE) || IS(t, T_ATTR_REF)  \
    || IS(t, T_ARRAY_REF) || IS(t, T_ARRAY_SLICE) || IS(t, T_CONCAT)  \
    || IS(t, T_TYPE_CONV))
#define IS_STMT(t)                                                    \
   (IS(t, T_PROCESS) || IS(t, T_WAIT) || IS(t, T_VAR_ASSIGN)          \
    || IS(t, T_SIGNAL_ASSIGN) || IS(t, T_ASSERT) || IS(t, T_INSTANCE) \
    || IS(t, T_IF) || IS(t, T_NULL) || IS(t, T_RETURN)                \
    || IS(t, T_CASSIGN) || IS(t, T_WHILE) || IS(t, T_FOR)             \
    || IS(t, T_EXIT) || IS(t, T_PCALL) || IS(t, T_CASE)               \
    || IS(t, T_BLOCK))
#define HAS_IDENT(t)                                                  \
   (IS(t, T_ENTITY) || IS(t, T_PORT_DECL) || IS(t, T_FCALL)           \
    || IS(t, T_ARCH) || IS(t, T_SIGNAL_DECL) || IS_STMT(t)            \
    || IS(t, T_VAR_DECL) || IS(t, T_REF) || IS(t, T_TYPE_DECL)        \
    || IS(t, T_PACKAGE) || IS(t, T_QUALIFIED) || IS(t, T_ENUM_LIT)    \
    || IS(t, T_CONST_DECL) || IS(t, T_FUNC_DECL) || IS(t, T_ELAB)     \
    || IS(t, T_ATTR_REF) || IS(t, T_INSTANCE) || IS(t, T_PACK_BODY)   \
    || IS(t, T_FUNC_BODY) || IS(t, T_CASSIGN) || IS(t, T_WHILE)       \
    || IS(t, T_ALIAS) || IS(t, T_ATTR_DECL) || IS(t, T_ATTR_SPEC)     \
    || IS(t, T_PROC_DECL) || IS(t, T_PROC_BODY) || IS(t, T_EXIT)      \
    || IS(t, T_PCALL) || IS(t, T_CASE) || IS(t, T_BLOCK))
#define HAS_IDENT2(t)                                                 \
   (IS(t, T_ARCH) || IS(t, T_ATTR_REF) || IS(t, T_INSTANCE)           \
    || IS(t, T_FOR) || IS(t, T_ATTR_SPEC) || IS(t, T_PCALL))
#define HAS_PORTS(t)                                                  \
   (IS(t, T_ENTITY) || IS(t, T_FUNC_DECL) || IS(t, T_FUNC_BODY)       \
    || IS(t, T_PROC_DECL) || IS(t, T_PROC_BODY))
#define HAS_GENERICS(t) (IS(t, T_ENTITY))
#define HAS_TYPE(t)                                                   \
   (IS(t, T_PORT_DECL) || IS(t, T_SIGNAL_DECL) || IS(t, T_VAR_DECL)   \
    || IS(t, T_TYPE_DECL) || IS_EXPR(t) || IS(t, T_ENUM_LIT)          \
    || IS(t, T_CONST_DECL) || IS(t, T_FUNC_DECL)                      \
    || IS(t, T_FUNC_BODY) || IS(t, T_ALIAS) || IS(t, T_ATTR_DECL)     \
    || IS(t, T_PROC_DECL) || IS(t, T_PROC_BODY))
#define HAS_PARAMS(t)                                                 \
   (IS(t, T_FCALL) || IS(t, T_ATTR_REF) || IS(t, T_ARRAY_REF)         \
    || IS(t, T_INSTANCE) || IS(t, T_PCALL) || IS(t, T_CONCAT)         \
    || IS(t, T_TYPE_CONV))
#define HAS_DECLS(t)                                                  \
   (IS(t, T_ARCH) || IS(t, T_PROCESS) || IS(t, T_PACKAGE)             \
    || IS(t, T_ELAB) || IS(t, T_PACK_BODY) || IS(t, T_FOR)            \
    || IS(t, T_FUNC_BODY) || IS(t, T_PROC_BODY) || IS(t, T_BLOCK))
#define HAS_TRIGGERS(t) (IS(t, T_WAIT) || IS(t, T_PROCESS))
#define HAS_STMTS(t)                                                  \
   (IS(t, T_ARCH) || IS(t, T_PROCESS) || IS(t, T_ELAB) || IS(t, T_IF) \
    || IS(t, T_FUNC_BODY) || IS(t, T_WHILE) || IS(t, T_FOR)           \
    || IS(t, T_PROC_BODY) || IS(t, T_BLOCK))
#define HAS_DELAY(t) (IS(t, T_WAIT) || IS(t, T_WAVEFORM))
#define HAS_TARGET(t)                                                 \
   (IS(t, T_VAR_ASSIGN) || IS(t, T_SIGNAL_ASSIGN)                     \
    || IS(t, T_CASSIGN))
#define HAS_VALUE(t)                                                  \
   (IS_DECL(t) || IS(t, T_VAR_ASSIGN) || IS(t, T_WAVEFORM)            \
    || IS(t, T_QUALIFIED) || IS(t, T_CONST_DECL) || IS(t, T_ASSERT)   \
    || IS(t, T_ATTR_REF) || IS(t, T_ARRAY_REF) || IS(t, T_CASE)       \
    || IS(t, T_ARRAY_SLICE) || IS(t, T_IF) || IS(t, T_RETURN)         \
    || IS(t, T_WHILE) || IS(t, T_ALIAS) || IS(t, T_ATTR_SPEC)         \
    || IS(t, T_EXIT) || IS(t, T_COND))
#define HAS_CONTEXT(t)                                                \
   (IS(t, T_ARCH) || IS(t, T_ENTITY) || IS(t, T_PACKAGE)              \
    || IS(t, T_PACK_BODY) || IS(t, T_ELAB))
#define HAS_REF(t)                                                    \
   (IS(t, T_REF) || IS(t, T_FCALL) || IS(t, T_ATTR_REF)               \
    || IS(t, T_ARRAY_REF) || IS(t, T_ARRAY_SLICE)                     \
    || IS(t, T_INSTANCE) || IS(t, T_PCALL) || IS(t, T_TYPE_CONV))
#define HAS_WAVEFORMS(t)                                              \
   (IS(t, T_SIGNAL_ASSIGN) || IS(t, T_COND))
#define HAS_RANGE(t) (IS(t, T_ARRAY_SLICE) || IS(t, T_FOR))
#define HAS_CLASS(t) (IS(t, T_PORT_DECL))
#define HAS_ASSOCS(t) (IS(t, T_AGGREGATE) || IS(t, T_CASE))
#define HAS_CONDS(t) (IS(t, T_CASSIGN))
#define HAS_REJECT(t) (IS(t, T_CASSIGN) || IS(t, T_SIGNAL_ASSIGN))

#define TREE_ARRAY_BASE_SZ  16

// Garbage collection
static tree_t *all_trees = NULL;
static size_t max_trees = 128;   // Grows at runtime
static size_t n_trees_alloc = 0;

unsigned next_generation = 1;

static unsigned tree_visit_aux(tree_t t, tree_visit_fn_t fn, void *context,
                               tree_kind_t kind, unsigned generation,
                               bool deep);

static void tree_array_add(struct tree_array *a, tree_t t)
{
   if (a->max == 0) {
      a->items = xmalloc(sizeof(tree_t) * TREE_ARRAY_BASE_SZ);
      a->max   = TREE_ARRAY_BASE_SZ;
   }
   else if (a->count == a->max) {
      a->max *= 2;
      a->items = xrealloc(a->items, sizeof(tree_t) * a->max);
   }

   a->items[a->count++] = t;
}

static inline tree_t tree_array_nth(struct tree_array *a, unsigned n)
{
   assert(n < a->count);
   return a->items[n];
}

static void param_array_add(struct param_array *a, param_t p)
{
   if (a->max == 0) {
      a->items = xmalloc(sizeof(param_t) * TREE_ARRAY_BASE_SZ);
      a->max   = TREE_ARRAY_BASE_SZ;
   }
   else if (a->count == a->max) {
      a->max *= 2;
      a->items = xrealloc(a->items, sizeof(param_t) * a->max);
   }

   a->items[a->count++] = p;
}

tree_t tree_new(tree_kind_t kind)
{
   assert(kind < T_LAST_TREE_KIND);

   tree_t t = xmalloc(sizeof(struct tree));
   memset(t, '\0', sizeof(struct tree));
   t->kind  = kind;
   t->index = UINT32_MAX;

   if (all_trees == NULL)
      all_trees = xmalloc(sizeof(tree_t) * max_trees);
   else if (n_trees_alloc == max_trees) {
      max_trees *= 2;
      all_trees = xrealloc(all_trees, sizeof(tree_t) * max_trees);
   }
   all_trees[n_trees_alloc++] = t;

   return t;
}

void tree_gc(void)
{
   // Generation will be updated by tree_visit
   const unsigned base_gen = next_generation;

   // Mark
   for (unsigned i = 0; i < n_trees_alloc; i++) {
      assert(all_trees[i] != NULL);

      if (IS_TOP_LEVEL(all_trees[i]))
         tree_visit_aux(all_trees[i], NULL, NULL,
                        (tree_kind_t)T_LAST_TREE_KIND,
                        next_generation++,
                        true);
   }

   // Sweep
   for (unsigned i = 0; i < n_trees_alloc; i++) {
      tree_t t = all_trees[i];
      if (t->generation < base_gen) {
         if (HAS_PORTS(t) && t->ports.items != NULL)
            free(t->ports.items);
         if (HAS_GENERICS(t) && t->generics.items != NULL)
            free(t->generics.items);
         if (HAS_PARAMS(t) && t->params.items != NULL)
            free(t->params.items);
         if (HAS_DECLS(t) && t->decls.items != NULL)
            free(t->decls.items);
         if (HAS_STMTS(t) && t->stmts.items != NULL)
            free(t->stmts.items);
         if (HAS_ASSOCS(t) && t->n_assocs_alloc > 0)
            free(t->assocs);

         if (HAS_CONTEXT(t) && t->context != NULL)
            free(t->context);

         if (t->attrs != NULL)
            free(t->attrs);

         free(t);

         all_trees[i] = NULL;
      }
   }

   // Compact
   size_t p = 0;
   for (unsigned i = 0; i < n_trees_alloc; i++) {
      if (all_trees[i] != NULL)
         all_trees[p++] = all_trees[i];
   }

   if (getenv("NVC_GC_VERBOSE") != NULL)
      printf("[gc: freed %zu trees; %zu allocated]\n",
             n_trees_alloc - p, p);

   n_trees_alloc = p;

   type_sweep(base_gen);
}

const loc_t *tree_loc(tree_t t)
{
   assert(t != NULL);

   return &t->loc;
}

void tree_set_loc(tree_t t, const loc_t *loc)
{
   assert(t != NULL);
   assert(loc != NULL);

   t->loc = *loc;
}

ident_t tree_ident(tree_t t)
{
   assert(t != NULL);
   assert(HAS_IDENT(t));
   assert(t->ident != NULL);

   return t->ident;
}

bool tree_has_ident(tree_t t)
{
   assert(t != NULL);
   assert(HAS_IDENT(t));

   return t->ident != NULL;
}

void tree_set_ident(tree_t t, ident_t i)
{
   assert(t != NULL);
   assert(i != NULL);
   assert(HAS_IDENT(t));

   t->ident = i;
}

ident_t tree_ident2(tree_t t)
{
   assert(t != NULL);
   assert(HAS_IDENT2(t));
   assert(t->ident2 != NULL);

   return t->ident2;
}

void tree_set_ident2(tree_t t, ident_t i)
{
   assert(t != NULL);
   assert(i != NULL);
   assert(HAS_IDENT2(t));

   t->ident2 = i;
}

tree_kind_t tree_kind(tree_t t)
{
   assert(t != NULL);
   return t->kind;
}

void tree_change_kind(tree_t t, tree_kind_t kind)
{
   assert(t != NULL);
   t->kind = kind;
}

unsigned tree_ports(tree_t t)
{
   assert(t != NULL);
   assert(HAS_PORTS(t));

   return t->ports.count;
}

tree_t tree_port(tree_t t, unsigned n)
{
   assert(t != NULL);
   assert(HAS_PORTS(t));

   return tree_array_nth(&t->ports, n);
}

void tree_add_port(tree_t t, tree_t d)
{
   assert(t != NULL);
   assert(d != NULL);
   assert(HAS_PORTS(t));
   assert(IS_DECL(d));

   tree_array_add(&t->ports, d);
}

port_mode_t tree_port_mode(tree_t t)
{
   assert(t != NULL);
   assert(IS(t, T_PORT_DECL));
   assert(t->port_mode != PORT_INVALID);

   return t->port_mode;
}

void tree_set_port_mode(tree_t t, port_mode_t mode)
{
   assert(t != NULL);
   assert(IS(t, T_PORT_DECL));

   t->port_mode = mode;
}

unsigned tree_generics(tree_t t)
{
   assert(t != NULL);
   assert(HAS_GENERICS(t));

   return t->generics.count;
}

tree_t tree_generic(tree_t t, unsigned n)
{
   assert(t != NULL);
   assert(HAS_GENERICS(t));

   return tree_array_nth(&t->generics, n);
}

void tree_add_generic(tree_t t, tree_t d)
{
   assert(t != NULL);
   assert(d != NULL);
   assert(HAS_GENERICS(t));
   assert(IS_DECL(d));

   tree_array_add(&t->generics, d);
}

type_t tree_type(tree_t t)
{
   assert(t != NULL);
   assert(HAS_TYPE(t));
   assert(t->type != NULL);

   return t->type;
}

void tree_set_type(tree_t t, type_t ty)
{
   assert(t != NULL);
   assert(HAS_TYPE(t));

   t->type = ty;
}

bool tree_has_type(tree_t t)
{
   assert(t != NULL);
   assert(HAS_TYPE(t));

   return t->type != NULL;
}

unsigned tree_params(tree_t t)
{
   assert(t != NULL);
   assert(HAS_PARAMS(t));

   return t->params.count;
}

param_t tree_param(tree_t t, unsigned n)
{
   assert(t != NULL);
   assert(HAS_PARAMS(t));
   assert(n < t->params.count);

   return t->params.items[n];
}

void tree_add_param(tree_t t, param_t e)
{
   assert(t != NULL);
   assert(HAS_PARAMS(t));
   assert(e.kind == P_RANGE || IS_EXPR(e.value));

   if (e.kind == P_POS)
      e.pos = t->params.count;

   param_array_add(&t->params, e);
}

unsigned tree_genmaps(tree_t t)
{
   assert(t != NULL);
   assert(IS(t, T_INSTANCE));

   return t->genmaps.count;
}

param_t tree_genmap(tree_t t, unsigned n)
{
   assert(t != NULL);
   assert(IS(t, T_INSTANCE));
   assert(n < t->genmaps.count);

   return t->genmaps.items[n];
}

void tree_add_genmap(tree_t t, param_t e)
{
   assert(t != NULL);
   assert(IS(t, T_INSTANCE));
   assert(e.kind == P_RANGE || IS_EXPR(e.value));

   if (e.kind == P_POS)
      e.pos = t->genmaps.count;

   param_array_add(&t->genmaps, e);
}

void tree_set_literal(tree_t t, literal_t lit)
{
   assert(t != NULL);
   assert(IS(t, T_LITERAL));

   t->literal = lit;
}

literal_t tree_literal(tree_t t)
{
   assert(t != NULL);
   assert(IS(t, T_LITERAL));

   return t->literal;
}

bool tree_has_value(tree_t t)
{
   assert(t != NULL);
   assert(HAS_VALUE(t));

   return t->value != NULL;
}

tree_t tree_value(tree_t t)
{
   assert(t != NULL);
   assert(HAS_VALUE(t));
   assert(t->value != NULL);

   return t->value;
}

void tree_set_value(tree_t t, tree_t v)
{
   assert(t != NULL);
   assert(HAS_VALUE(t));
   assert(v == NULL || IS_EXPR(v));

   t->value = v;
}

unsigned tree_decls(tree_t t)
{
   assert(t != NULL);
   assert(HAS_DECLS(t));

   return t->decls.count;
}

tree_t tree_decl(tree_t t, unsigned n)
{
   assert(t != NULL);
   assert(HAS_DECLS(t));

   return tree_array_nth(&t->decls, n);
}

void tree_add_decl(tree_t t, tree_t d)
{
   assert(t != NULL);
   assert(d != NULL);
   assert(HAS_DECLS(t));
   assert(IS_DECL(d));

   tree_array_add(&t->decls, d);
}

unsigned tree_stmts(tree_t t)
{
   assert(t != NULL);
   assert(HAS_STMTS(t));

   return t->stmts.count;
}

tree_t tree_stmt(tree_t t, unsigned n)
{
   assert(t != NULL);
   assert(HAS_STMTS(t));

   return tree_array_nth(&t->stmts, n);
}

void tree_add_stmt(tree_t t, tree_t s)
{
   assert(t != NULL);
   assert(s != NULL);
   assert(HAS_STMTS(t));
   assert(IS_STMT(s));

   tree_array_add(&t->stmts, s);
}

unsigned tree_waveforms(tree_t t)
{
   assert(t != NULL);
   assert(HAS_WAVEFORMS(t));

   return t->waves.count;
}

tree_t tree_waveform(tree_t t, unsigned n)
{
   assert(t != NULL);
   assert(HAS_WAVEFORMS(t));

   return tree_array_nth(&t->waves, n);
}

void tree_add_waveform(tree_t t, tree_t w)
{
   assert(t != NULL);
   assert(w != NULL);
   assert(HAS_WAVEFORMS(t));
   assert(IS(w, T_WAVEFORM));

   tree_array_add(&t->waves, w);
}

unsigned tree_else_stmts(tree_t t)
{
   assert(t != NULL);
   assert(IS(t, T_IF));

   return t->elses.count;
}

tree_t tree_else_stmt(tree_t t, unsigned n)
{
   assert(t != NULL);
   assert(IS(t, T_IF));

   return tree_array_nth(&t->elses, n);
}

void tree_add_else_stmt(tree_t t, tree_t s)
{
   assert(t != NULL);
   assert(s != NULL);
   assert(IS(t, T_IF));
   assert(IS_STMT(s));

   tree_array_add(&t->elses, s);
}

unsigned tree_conds(tree_t t)
{
   assert(t != NULL);
   assert(HAS_CONDS(t));

   return t->conds.count;
}

tree_t tree_cond(tree_t t, unsigned n)
{
   assert(t != NULL);
   assert(HAS_CONDS(t));

   return tree_array_nth(&t->conds, n);
}

void tree_add_cond(tree_t t, tree_t c)
{
   assert(t != NULL);
   assert(c != NULL);
   assert(HAS_CONDS(t));
   assert(IS(c, T_COND));

   tree_array_add(&t->conds, c);
}

unsigned tree_drivers(tree_t t)
{
   assert(t != NULL);
   assert(IS(t, T_SIGNAL_DECL));

   return t->drivers.count;
}

tree_t tree_driver(tree_t t, unsigned n)
{
   assert(t != NULL);
   assert(IS(t, T_SIGNAL_DECL));

   return tree_array_nth(&t->drivers, n);
}

void tree_add_driver(tree_t t, tree_t d)
{
   assert(t != NULL);
   assert(d != NULL);
   assert(IS(t, T_SIGNAL_DECL));
   assert(IS(d, T_PROCESS));

   tree_array_add(&t->drivers, d);
}

unsigned tree_sub_drivers(tree_t t, unsigned elem)
{
   assert(t != NULL);
   assert(IS(t, T_SIGNAL_DECL));

   if (t->sub_drivers == NULL || elem >= t->n_elems)
      return 0;
   else
      return t->sub_drivers[elem].count;
}

tree_t tree_sub_driver(tree_t t, unsigned elem, unsigned n)
{
   assert(t != NULL);
   assert(IS(t, T_SIGNAL_DECL));
   assert(elem < t->n_elems);

   return tree_array_nth(&t->sub_drivers[elem], n);
}

void tree_add_sub_driver(tree_t t, unsigned elem, tree_t p)
{
   assert(t != NULL);
   assert(IS(t, T_SIGNAL_DECL));
   assert(IS(p, T_PROCESS));

   if (elem >= t->n_elems) {
      // TODO: growing by 1 each time is pretty inefficient
      //  -> add tree_sub_driver_hint(tree_t, unsigned)
      t->sub_drivers = xrealloc(t->sub_drivers,
                                (elem + 1) * sizeof(struct tree_array));
      memset(&t->sub_drivers[t->n_elems], '\0',
             (elem + 1 - t->n_elems) * sizeof(struct tree_array));
      t->n_elems = elem + 1;
   }

   tree_array_add(&t->sub_drivers[elem], p);
}

bool tree_has_delay(tree_t t)
{
   assert(t != NULL);
   assert(HAS_DELAY(t));

   return t->delay != NULL;
}

tree_t tree_delay(tree_t t)
{
   assert(t != NULL);
   assert(HAS_DELAY(t));
   assert(t->delay != NULL);

   return t->delay;
}

void tree_set_delay(tree_t t, tree_t d)
{
   assert(t != NULL);
   assert(d != NULL);
   assert(HAS_DELAY(t));
   assert(IS_EXPR(d));

   t->delay = d;
}

unsigned tree_triggers(tree_t t)
{
   assert(t != NULL);
   assert(HAS_TRIGGERS(t));

   return t->triggers.count;
}

tree_t tree_trigger(tree_t t, unsigned n)
{
   assert(t != NULL);
   assert(HAS_TRIGGERS(t));

   return tree_array_nth(&t->triggers, n);
}

void tree_add_trigger(tree_t t, tree_t s)
{
   assert(t != NULL);
   assert(s != NULL);
   assert(HAS_TRIGGERS(t));
   assert(IS_EXPR(s));

   tree_array_add(&t->triggers, s);
}

tree_t tree_target(tree_t t)
{
   assert(t != NULL);
   assert(HAS_TARGET(t));
   assert(t->target != NULL);

   return t->target;
}

void tree_set_target(tree_t t, tree_t lhs)
{
   assert(t != NULL);
   assert(HAS_TARGET(t));

   t->target = lhs;
}

tree_t tree_ref(tree_t t)
{
   assert(t != NULL);
   assert(HAS_REF(t));
   assert(t->ref != NULL);

   return t->ref;
}

void tree_set_ref(tree_t t, tree_t decl)
{
   assert(t != NULL);
   assert(HAS_REF(t));
   assert(IS_DECL(decl) || IS(decl, T_ENUM_LIT) || IS_TOP_LEVEL(decl));

   t->ref = decl;
}

unsigned tree_contexts(tree_t t)
{
   assert(t != NULL);
   assert(HAS_CONTEXT(t));

   return t->n_contexts;
}

context_t tree_context(tree_t t, unsigned n)
{
   assert(t != NULL);
   assert(HAS_CONTEXT(t));
   assert(n < t->n_contexts);

   return t->context[n];
}

void tree_add_context(tree_t t, context_t ctx)
{
   assert(t != NULL);
   assert(HAS_CONTEXT(t));
   assert(t->n_contexts < MAX_CONTEXTS);

   for (unsigned i = 0; i < tree_contexts(t); i++) {
      if (t->context[i].name == ctx.name)
         return;
   }

   if (t->n_contexts == 0)
      t->context = xmalloc(sizeof(context_t) * MAX_CONTEXTS);

   t->context[t->n_contexts++] = ctx;
}

unsigned tree_assocs(tree_t t)
{
   assert(t != NULL);
   assert(HAS_ASSOCS(t));

   return t->n_assocs;
}

assoc_t tree_assoc(tree_t t, unsigned n)
{
   assert(t != NULL);
   assert(HAS_ASSOCS(t));
   assert(n < t->n_assocs);

   return t->assocs[n];
}

void tree_change_assoc(tree_t t, unsigned i, assoc_t a)
{
   assert(t != NULL);
   assert(HAS_ASSOCS(t));
   assert(i < t->n_assocs);

   t->assocs[i] = a;
}

void tree_add_assoc(tree_t t, assoc_t a)
{
   assert(t != NULL);
   assert(HAS_ASSOCS(t));

   if (t->assocs == NULL) {
      t->n_assocs_alloc = 16;
      t->assocs = xmalloc(sizeof(assoc_t) * t->n_assocs_alloc);
   }
   else if (t->n_assocs == t->n_assocs_alloc) {
      t->n_assocs_alloc *= 2;
      t->assocs = xrealloc(t->assocs, sizeof(assoc_t) * t->n_assocs_alloc);
   }

   if (a.kind == A_POS) {
      unsigned pos = 0;
      for (unsigned i = 0; i < t->n_assocs; i++) {
         if (t->assocs[i].kind == A_POS)
            pos++;
      }

      a.pos = pos;
   }

   t->assocs[t->n_assocs++] = a;
}

tree_t tree_severity(tree_t t)
{
   assert(t != NULL);
   assert(IS(t, T_ASSERT));
   assert(t->severity != NULL);

   return t->severity;
}

void tree_set_severity(tree_t t, tree_t s)
{
   assert(t != NULL);
   assert(IS(t, T_ASSERT));
   assert(IS_EXPR(s));

   t->severity = s;
}

tree_t tree_message(tree_t t)
{
   assert(t != NULL);
   assert(IS(t, T_ASSERT));
   assert(t->message != NULL);

   return t->message;
}

void tree_set_message(tree_t t, tree_t m)
{
   assert(t != NULL);
   assert(IS(t, T_ASSERT));
   assert(IS_EXPR(m));

   t->message = m;
}

range_t tree_range(tree_t t)
{
   assert(t != NULL);
   assert(HAS_RANGE(t));

   return t->range;
}

void tree_set_range(tree_t t, range_t r)
{
   assert(t != NULL);
   assert(HAS_RANGE(t));

   t->range = r;
}

unsigned tree_pos(tree_t t)
{
   assert(t != NULL);
   assert(IS(t, T_ENUM_LIT));

   return t->pos;
}

void tree_set_pos(tree_t t, unsigned pos)
{
   assert(t != NULL);
   assert(IS(t, T_ENUM_LIT));

   t->pos = pos;
}

class_t tree_class(tree_t t)
{
   assert(t != NULL);
   assert(HAS_CLASS(t));

   return t->class;
}

void tree_set_class(tree_t t, class_t c)
{
   assert(t != NULL);
   assert(HAS_CLASS(t));

   t->class = c;
}

tree_t tree_reject(tree_t t)
{
   assert(t != NULL);
   assert(HAS_REJECT(t));
   assert(t->reject != NULL);

   return t->reject;
}

void tree_set_reject(tree_t t, tree_t r)
{
   assert(t != NULL);
   assert(HAS_REJECT(t));
   assert(IS_EXPR(r));

   t->reject = r;
}

uint32_t tree_index(tree_t t)
{
   assert(t != NULL);
   assert(t->index != UINT32_MAX);

   return t->index;
}

static unsigned tree_visit_a(struct tree_array *a,
                             tree_visit_fn_t fn, void *context,
                             tree_kind_t kind, unsigned generation,
                             bool deep)
{
   unsigned n = 0;
   for (unsigned i = 0; i < a->count; i++)
      n += tree_visit_aux(a->items[i], fn, context, kind,
                          generation, deep);

   return n;
}

static unsigned tree_visit_p(struct param_array *a,
                             tree_visit_fn_t fn, void *context,
                             tree_kind_t kind, unsigned generation,
                             bool deep)
{
   unsigned n = 0;
   for (unsigned i = 0; i < a->count; i++) {
      switch (a->items[i].kind) {
      case P_RANGE:
         n += tree_visit_aux(a->items[i].range.left,
                             fn, context, kind, generation, deep);
         n += tree_visit_aux(a->items[i].range.right,
                             fn, context, kind, generation, deep);
         break;

      case P_POS:
      case P_NAMED:
         n += tree_visit_aux(a->items[i].value,
                             fn, context, kind, generation, deep);
         break;
      }
   }

   return n;
}

static unsigned tree_visit_type(type_t type,
                                tree_visit_fn_t fn, void *context,
                                tree_kind_t kind, unsigned generation,
                                bool deep)
{
   if (type == NULL)
      return 0;

   if (!type_update_generation(type, generation))
      return 0;

   unsigned n = 0;

   switch (type_kind(type)) {
   case T_SUBTYPE:
   case T_INTEGER:
   case T_PHYSICAL:
   case T_CARRAY:
      for (unsigned i = 0; i < type_dims(type); i++) {
         range_t r = type_dim(type, i);
         n += tree_visit_aux(r.left, fn, context, kind,
                             generation, deep);
         n += tree_visit_aux(r.right, fn, context, kind,
                             generation, deep);
      }
      break;

   default:
      break;
   }

   switch (type_kind(type)) {
   case T_SUBTYPE:
   case T_CARRAY:
   case T_UARRAY:
      n += tree_visit_type(type_base(type), fn, context, kind,
                           generation, deep);
      break;
   default:
      break;
   }

   switch (type_kind(type)) {
   case T_UNRESOLVED:
      break;

   case T_SUBTYPE:
      if (type_has_resolution(type))
         n += tree_visit_aux(type_resolution(type), fn, context,
                             kind, generation, deep);
      break;

   case T_PHYSICAL:
      for (unsigned i = 0; i < type_units(type); i++)
         n += tree_visit_aux(type_unit(type, i).multiplier, fn, context,
                             kind, generation, deep);
      break;

   case T_FUNC:
      for (unsigned i = 0; i < type_params(type); i++)
         n += tree_visit_type(type_param(type, i), fn, context,
                              kind, generation, deep);
      n += tree_visit_type(type_result(type), fn, context,
                           kind, generation, deep);
      break;

   case T_ENUM:
      for (unsigned i = 0; i < type_enum_literals(type); i++)
         n += tree_visit_aux(type_enum_literal(type, i), fn, context,
                             kind, generation, deep);
      break;

   case T_UARRAY:
      for (unsigned i = 0; i < type_index_constrs(type); i++)
         n += tree_visit_type(type_index_constr(type, i),
                              fn, context, kind, generation, deep);
      break;

   default:
      break;
   }

   return n;
}

static unsigned tree_visit_aux(tree_t t, tree_visit_fn_t fn, void *context,
                               tree_kind_t kind, unsigned generation,
                               bool deep)
{
   // If `deep' then will follow links above the tree originally passed
   // to tree_visit - e.g. following references back to their declarations
   // Outside the garbage collector this is usually not what is required

   if (t == NULL || t->generation == generation)
      return 0;

   t->generation = generation;

   unsigned n = 0;

   if (HAS_PORTS(t))
      n += tree_visit_a(&t->ports, fn, context, kind, generation, deep);
   if (HAS_GENERICS(t))
      n += tree_visit_a(&t->generics, fn, context, kind, generation, deep);
   if (HAS_DECLS(t))
      n += tree_visit_a(&t->decls, fn, context, kind, generation, deep);
   if (HAS_TRIGGERS(t))
      n += tree_visit_a(&t->triggers, fn, context, kind, generation, deep);
   if (HAS_STMTS(t))
      n += tree_visit_a(&t->stmts, fn, context, kind, generation, deep);
   if (HAS_CONDS(t))
      n += tree_visit_a(&t->conds, fn, context, kind, generation, deep);
   if (HAS_WAVEFORMS(t))
      n += tree_visit_a(&t->waves, fn, context, kind, generation, deep);
   if (HAS_VALUE(t))
      n += tree_visit_aux(t->value, fn, context, kind, generation, deep);
   if (HAS_DELAY(t))
      n += tree_visit_aux(t->delay, fn, context, kind, generation, deep);
   if (HAS_REJECT(t))
      n += tree_visit_aux(t->reject, fn, context, kind, generation, deep);
   if (HAS_TARGET(t))
      n += tree_visit_aux(t->target, fn, context, kind, generation, deep);
   if (HAS_REF(t) && deep)
      n += tree_visit_aux(t->ref, fn, context, kind, generation, deep);
   if (HAS_TYPE(t) && deep)
      n += tree_visit_type(t->type, fn, context, kind, generation, deep);
   if (HAS_PARAMS(t))
      n += tree_visit_p(&t->params, fn, context, kind, generation, deep);
   if (HAS_RANGE(t)) {
      n += tree_visit_aux(t->range.left, fn, context, kind,
                          generation, deep);
      n += tree_visit_aux(t->range.right, fn, context, kind,
                          generation, deep);
   }
   if (HAS_ASSOCS(t)) {
      for (unsigned i = 0; i < t->n_assocs; i++) {
         switch (t->assocs[i].kind) {
         case A_NAMED:
            n += tree_visit_aux(t->assocs[i].name, fn, context,
                                kind, generation, deep);
            break;
         case A_RANGE:
            n += tree_visit_aux(t->assocs[i].range.left, fn, context,
                                kind, generation, deep);
            n += tree_visit_aux(t->assocs[i].range.right, fn, context,
                                kind, generation, deep);
            break;
         default:
            break;
         }

         n += tree_visit_aux(t->assocs[i].value, fn, context,
                             kind, generation, deep);
      }
   }

   if (IS(t, T_ASSERT)) {
      n += tree_visit_aux(t->severity, fn, context,
                          kind, generation, deep);
      n += tree_visit_aux(t->message, fn, context,
                          kind, generation, deep);
   }
   else if (IS(t, T_SIGNAL_DECL) && deep) {
      n += tree_visit_a(&t->drivers, fn, context, kind, generation, deep);
      for (unsigned i = 0; i < t->n_elems; i++)
         n += tree_visit_a(&t->sub_drivers[i], fn, context,
                           kind, generation, deep);
   }
   else if (IS(t, T_INSTANCE))
      n += tree_visit_p(&t->genmaps, fn, context, kind, generation, deep);
   else if (IS(t, T_IF))
      n += tree_visit_a(&t->elses, fn, context, kind, generation, deep);

   if (deep) {
      for (unsigned i = 0; i < t->n_attrs; i++) {
         switch (t->attrs[i].kind) {
         case A_TREE:
            n += tree_visit_aux(t->attrs[i].tval, fn, context,
                                kind, generation, deep);
            break;

         default:
            break;
         }
      }
   }

   if (t->kind == kind || kind == T_LAST_TREE_KIND) {
      if (fn)
         (*fn)(t, context);
      ++n;
   }

   return n;
}

unsigned tree_visit(tree_t t, tree_visit_fn_t fn, void *context)
{
   assert(t != NULL);

   return tree_visit_aux(t, fn, context,
                         (tree_kind_t)T_LAST_TREE_KIND,
                         next_generation++,
                         false);
}

unsigned tree_visit_only(tree_t t, tree_visit_fn_t fn,
                         void *context, tree_kind_t kind)
{
   assert(t != NULL);

   return tree_visit_aux(t, fn, context, kind,
                         next_generation++, false);
}

static void write_loc(loc_t *l, tree_wr_ctx_t ctx)
{
   const char *fname = (l->file != NULL ? l->file : "(none)");

   uint8_t findex;
   for (findex = 0;
        (findex < 0xff)
           && (ctx->file_names[findex] != NULL)
           && (strcmp(ctx->file_names[findex], fname) != 0);
        findex++)
      ;
   assert(findex != 0xff);

   if (ctx->file_names[findex] == NULL) {
      const size_t len = strlen(fname) + 1;

      ctx->file_names[findex] = fname;

      write_u8(0xff, ctx->file);
      write_u8(findex, ctx->file);
      write_u16(len, ctx->file);
      if (fwrite(fname, 1, len, ctx->file) != len)
         fatal("fwrite failed");
   }
   else
      write_u8(findex, ctx->file);

   write_u16(l->first_line, ctx->file);
   write_u16(l->first_column, ctx->file);
   write_u16(l->last_line, ctx->file);
   write_u16(l->last_column, ctx->file);
}

static loc_t read_loc(tree_rd_ctx_t ctx)
{
   const char *fname;
   uint8_t fmarker = read_u8(ctx->file);
   if (fmarker == 0xff) {
      uint8_t index = read_u8(ctx->file);
      uint16_t len = read_u16(ctx->file);
      char *buf = xmalloc(len);
      if (fread(buf, len, 1, ctx->file) != 1)
         fatal("premature end of file");

      ctx->file_names[index] = buf;
      fname = buf;
   }
   else {
      fname = ctx->file_names[fmarker];
      assert(fname != NULL);
   }

   loc_t l = { .file = fname, .linebuf = NULL };
   l.first_line   = read_u16(ctx->file);
   l.first_column = read_u16(ctx->file);
   l.last_line    = read_u16(ctx->file);
   l.last_column  = read_u16(ctx->file);
   return l;
}

static void write_a(struct tree_array *a, tree_wr_ctx_t ctx)
{
   write_u32(a->count, ctx->file);
   for (unsigned i = 0; i < a->count; i++)
      tree_write(a->items[i], ctx);
}

static void read_a(struct tree_array *a, tree_rd_ctx_t ctx)
{
   a->count = a->max = read_u32(ctx->file);
   a->items = xmalloc(a->count * sizeof(tree_t));
   for (unsigned i = 0; i < a->count; i++)
      a->items[i] = tree_read(ctx);
}

static void write_p(struct param_array *a, tree_wr_ctx_t ctx)
{
   write_u32(a->count, ctx->file);
   for (unsigned i = 0; i < a->count; i++) {
      write_u16(a->items[i].kind, ctx->file);
      switch (a->items[i].kind) {
      case P_POS:
         write_u16(a->items[i].pos, ctx->file);
         tree_write(a->items[i].value, ctx);
         break;
      case P_RANGE:
         write_u16(a->items[i].range.kind, ctx->file);
         tree_write(a->items[i].range.left, ctx);
         tree_write(a->items[i].range.right, ctx);
         break;
      case P_NAMED:
         ident_write(a->items[i].name, ctx->ident_ctx);
         tree_write(a->items[i].value, ctx);
         break;
      }
   }
}

static void read_p(struct param_array *a, tree_rd_ctx_t ctx)
{
   a->max = a->count = read_u32(ctx->file);
   a->items = xmalloc(sizeof(param_t) * a->count);

   for (unsigned i = 0; i < a->count; i++) {
      switch ((a->items[i].kind = read_u16(ctx->file))) {
      case P_POS:
         a->items[i].pos   = read_u16(ctx->file);
         a->items[i].value = tree_read(ctx);
         break;
      case P_RANGE:
         a->items[i].range.kind  = read_u16(ctx->file);
         a->items[i].range.left  = tree_read(ctx);
         a->items[i].range.right = tree_read(ctx);
         break;
      case P_NAMED:
         a->items[i].name  = ident_read(ctx->ident_ctx);
         a->items[i].value = tree_read(ctx);
         break;
      }
   }
}

tree_wr_ctx_t tree_write_begin(FILE *f)
{
   write_u16(FILE_FMT_VER, f);

   struct tree_wr_ctx *ctx = xmalloc(sizeof(struct tree_wr_ctx));
   ctx->file       = f;
   ctx->generation = next_generation++;
   ctx->n_trees    = 0;
   ctx->ident_ctx  = ident_write_begin(f);
   ctx->type_ctx   = type_write_begin(ctx, ctx->ident_ctx);
   memset(ctx->file_names, '\0', sizeof(ctx->file_names));

   return ctx;
}

void tree_write_end(tree_wr_ctx_t ctx)
{
   ident_write_end(ctx->ident_ctx);
   type_write_end(ctx->type_ctx);
   free(ctx);
}

FILE *tree_write_file(tree_wr_ctx_t ctx)
{
   return ctx->file;
}

void tree_write(tree_t t, tree_wr_ctx_t ctx)
{
   if (t == NULL) {
      write_u16(0xffff, ctx->file);  // Null marker
      return;
   }

   if (t->generation == ctx->generation) {
      // Already visited this tree
      write_u16(0xfffe, ctx->file);   // Back reference marker
      write_u32(t->index, ctx->file);
      return;
   }

   t->generation = ctx->generation;
   t->index      = (ctx->n_trees)++;

   write_u16(t->kind, ctx->file);
   write_loc(&t->loc, ctx);
   if (HAS_IDENT(t))
      ident_write(t->ident, ctx->ident_ctx);
   if (HAS_IDENT2(t))
      ident_write(t->ident2, ctx->ident_ctx);
   if (HAS_PORTS(t))
      write_a(&t->ports, ctx);
   if (HAS_GENERICS(t))
      write_a(&t->generics, ctx);
   if (HAS_DECLS(t))
      write_a(&t->decls, ctx);
   if (HAS_TRIGGERS(t))
      write_a(&t->triggers, ctx);
   if (HAS_STMTS(t))
      write_a(&t->stmts, ctx);
   if (HAS_WAVEFORMS(t))
      write_a(&t->waves, ctx);
   if (HAS_CONDS(t))
      write_a(&t->conds, ctx);
   if (HAS_TYPE(t))
      type_write(t->type, ctx->type_ctx);
   if (HAS_VALUE(t))
      tree_write(t->value, ctx);
   if (HAS_DELAY(t))
      tree_write(t->delay, ctx);
   if (HAS_REJECT(t))
      tree_write(t->reject, ctx);
   if (HAS_TARGET(t))
      tree_write(t->target, ctx);
   if (HAS_REF(t))
      tree_write(t->ref, ctx);
   if (HAS_CONTEXT(t)) {
      write_u16(t->n_contexts, ctx->file);
      for (unsigned i = 0; i < t->n_contexts; i++) {
         ident_write(t->context[i].name, ctx->ident_ctx);
         write_loc(&t->context[i].loc, ctx);
      }
   }
   if (HAS_PARAMS(t))
      write_p(&t->params, ctx);
   if (HAS_RANGE(t)) {
      write_u16(t->range.kind, ctx->file);
      tree_write(t->range.left, ctx);
      tree_write(t->range.right, ctx);
   }
   if (HAS_CLASS(t))
      write_u16(t->class, ctx->file);
   if (HAS_ASSOCS(t)) {
      write_u16(t->n_assocs, ctx->file);

      for (unsigned i = 0; i < t->n_assocs; i++) {
         write_u16(t->assocs[i].kind, ctx->file);
         tree_write(t->assocs[i].value, ctx);

         switch (t->assocs[i].kind) {
         case A_POS:
            write_u16(t->assocs[i].pos, ctx->file);
            break;
         case A_NAMED:
            tree_write(t->assocs[i].name, ctx);
            break;
         case A_RANGE:
            write_u16(t->assocs[i].range.kind, ctx->file);
            tree_write(t->assocs[i].range.left, ctx);
            tree_write(t->assocs[i].range.right, ctx);
            break;
         case A_OTHERS:
            break;
         default:
            abort();
         }
      }
   }

   switch (t->kind) {
   case T_PORT_DECL:
      write_u16(t->port_mode, ctx->file);
      break;

   case T_LITERAL:
      {
         write_u16(t->literal.kind, ctx->file);
         switch (t->literal.kind) {
         case L_INT:
            write_i64(t->literal.i, ctx->file);
            break;
         default:
            abort();
         }
      }
      break;

   case T_ASSERT:
      tree_write(t->severity, ctx);
      tree_write(t->message, ctx);
      break;

   case T_ENUM_LIT:
      write_u32(t->pos, ctx->file);
      break;

   case T_INSTANCE:
      write_p(&t->genmaps, ctx);
      break;

   case T_IF:
      write_a(&t->elses, ctx);
      break;

   default:
      break;
   }

   write_u16(t->n_attrs, ctx->file);
   for (unsigned i = 0; i < t->n_attrs; i++) {
      write_u16(t->attrs[i].kind, ctx->file);
      ident_write(t->attrs[i].name, ctx->ident_ctx);

      switch (t->attrs[i].kind) {
      case A_STRING:
         ident_write(t->attrs[i].sval, ctx->ident_ctx);
         break;

      case A_INT:
         write_i32(t->attrs[i].ival, ctx->file);
         break;

      case A_TREE:
         tree_write(t->attrs[i].tval, ctx);
         break;

      case A_PTR:
         fatal("pointer attributes cannot be saved");
      }
   }

#ifdef EXTRA_READ_CHECKS
   write_u16(0xdead, ctx->file);
#endif  // EXTRA_READ_CHECKS
}

tree_t tree_read(tree_rd_ctx_t ctx)
{
   uint16_t marker = read_u16(ctx->file);
   if (marker == 0xffff)
      return NULL;    // Null marker
   else if (marker == 0xfffe) {
      // Back reference marker
      unsigned index = read_u32(ctx->file);
      assert(index < ctx->n_trees);
      return ctx->store[index];
   }

   assert(marker < T_LAST_TREE_KIND);

   tree_t t = tree_new((tree_kind_t)marker);
   t->loc = read_loc(ctx);

   // Stash pointer for later back references
   // This must be done early as a child node of this type may
   // reference upwards
   t->index = ctx->n_trees++;
   if (ctx->n_trees == ctx->store_sz) {
      ctx->store_sz *= 2;
      ctx->store = xrealloc(ctx->store, ctx->store_sz * sizeof(tree_t));
   }
   ctx->store[t->index] = t;

   if (HAS_IDENT(t))
      tree_set_ident(t, ident_read(ctx->ident_ctx));
   if (HAS_IDENT2(t))
      tree_set_ident2(t, ident_read(ctx->ident_ctx));
   if (HAS_PORTS(t))
      read_a(&t->ports, ctx);
   if (HAS_GENERICS(t))
      read_a(&t->generics, ctx);
   if (HAS_DECLS(t))
      read_a(&t->decls, ctx);
   if (HAS_TRIGGERS(t))
      read_a(&t->triggers, ctx);
   if (HAS_STMTS(t))
      read_a(&t->stmts, ctx);
   if (HAS_WAVEFORMS(t))
      read_a(&t->waves, ctx);
   if (HAS_CONDS(t))
      read_a(&t->conds, ctx);
   if (HAS_TYPE(t))
      t->type = type_read(ctx->type_ctx);
   if (HAS_VALUE(t))
      t->value = tree_read(ctx);
   if (HAS_DELAY(t))
      t->delay = tree_read(ctx);
   if (HAS_REJECT(t))
      t->reject = tree_read(ctx);
   if (HAS_TARGET(t))
      t->target = tree_read(ctx);
   if (HAS_REF(t))
      t->ref = tree_read(ctx);
   if (HAS_CONTEXT(t)) {
      t->n_contexts = read_u16(ctx->file);
      t->context    = xmalloc(sizeof(context_t) * MAX_CONTEXTS);

      for (unsigned i = 0; i < t->n_contexts; i++) {
         t->context[i].name = ident_read(ctx->ident_ctx);
         t->context[i].loc  = read_loc(ctx);
      }
   }
   if (HAS_PARAMS(t))
      read_p(&t->params, ctx);
   if (HAS_RANGE(t)) {
      t->range.kind  = read_u16(ctx->file);
      t->range.left  = tree_read(ctx);
      t->range.right = tree_read(ctx);
   }
   if (HAS_CLASS(t))
      t->class = read_u16(ctx->file);
   if (HAS_ASSOCS(t)) {
      t->n_assocs_alloc = t->n_assocs = read_u16(ctx->file);
      t->assocs = xmalloc(sizeof(assoc_t) * t->n_assocs);

      for (unsigned i = 0; i < t->n_assocs; i++) {
         t->assocs[i].kind  = read_u16(ctx->file);
         t->assocs[i].value = tree_read(ctx);

         switch (t->assocs[i].kind) {
         case A_POS:
            t->assocs[i].pos = read_u16(ctx->file);
            break;
         case A_NAMED:
            t->assocs[i].name = tree_read(ctx);
            break;
         case A_RANGE:
            t->assocs[i].range.kind  = read_u16(ctx->file);
            t->assocs[i].range.left  = tree_read(ctx);
            t->assocs[i].range.right = tree_read(ctx);
            break;
         case A_OTHERS:
            break;
         default:
            abort();
         }
      }
   }

   switch (t->kind) {
   case T_PORT_DECL:
      t->port_mode = read_u16(ctx->file);
      break;

   case T_LITERAL:
      {
         t->literal.kind = read_u16(ctx->file);
         switch (t->literal.kind) {
         case L_INT:
            t->literal.i = read_i64(ctx->file);
            break;
         default:
            abort();
         }
      }
      break;

   case T_ASSERT:
      t->severity = tree_read(ctx);
      t->message  = tree_read(ctx);
      break;

   case T_ENUM_LIT:
      t->pos = read_u32(ctx->file);
      break;

   case T_INSTANCE:
      read_p(&t->genmaps, ctx);
      break;

   case T_IF:
      read_a(&t->elses, ctx);
      break;

   default:
      break;
   }

   t->n_attrs = read_u16(ctx->file);
   assert(t->n_attrs <= MAX_ATTRS);
   t->attrs = xmalloc(sizeof(struct attr) * MAX_ATTRS);

   for (unsigned i = 0; i < t->n_attrs; i++) {
      t->attrs[i].kind = read_u16(ctx->file);
      t->attrs[i].name = ident_read(ctx->ident_ctx);

      switch (t->attrs[i].kind) {
      case A_STRING:
         t->attrs[i].sval = ident_read(ctx->ident_ctx);
         break;

      case A_INT:
         t->attrs[i].ival = read_i32(ctx->file);
         break;

      case A_TREE:
         t->attrs[i].tval = tree_read(ctx);
         break;

      default:
         abort();
      }
   }

#ifdef EXTRA_READ_CHECKS
   unsigned short term = read_u16(ctx->file);
   if (term != 0xdead)
      fatal("bad tree termination marker %x kind=%d",
            term, t->kind);
#endif  // EXTRA_READ_CHECKS

   return t;
}

tree_rd_ctx_t tree_read_begin(FILE *f, const char *fname)
{
   uint16_t ver = read_u16(f);
   if (ver != FILE_FMT_VER)
      fatal("%s: serialised version %x expected %x",
            fname, ver, FILE_FMT_VER);

   struct tree_rd_ctx *ctx = xmalloc(sizeof(struct tree_rd_ctx));
   ctx->file      = f;
   ctx->ident_ctx = ident_read_begin(f);
   ctx->type_ctx  = type_read_begin(ctx, ctx->ident_ctx);
   ctx->store_sz  = 128;
   ctx->store     = xmalloc(ctx->store_sz * sizeof(tree_t));
   ctx->n_trees   = 0;
   ctx->db_fname  = strdup(fname);
   memset(ctx->file_names, '\0', sizeof(ctx->file_names));

   return ctx;
}

void tree_read_end(tree_rd_ctx_t ctx)
{
   ident_read_end(ctx->ident_ctx);
   type_read_end(ctx->type_ctx);
   fclose(ctx->file);
   free(ctx->store);
   free(ctx->db_fname);
   free(ctx);
}

FILE *tree_read_file(tree_rd_ctx_t ctx)
{
   return ctx->file;
}

tree_t tree_read_recall(tree_rd_ctx_t ctx, uint32_t index)
{
   assert(index < ctx->n_trees);
   return ctx->store[index];
}

static struct attr *tree_find_attr(tree_t t, ident_t name, attr_kind_t kind)
{
   assert(t != NULL);

   for (unsigned i = 0; i < t->n_attrs; i++) {
      if (t->attrs[i].kind == kind && t->attrs[i].name == name)
         return &t->attrs[i];
   }

   return NULL;
}

static struct attr *tree_add_attr(tree_t t, ident_t name, attr_kind_t kind)
{
   assert(t != NULL);
   assert(t->n_attrs < MAX_ATTRS);

   struct attr *a = tree_find_attr(t, name, kind);
   if (a != NULL)
      return a;

   if (t->attrs == NULL)
      t->attrs = xmalloc(sizeof(struct attr) * MAX_ATTRS);

   unsigned i = t->n_attrs++;
   t->attrs[i].kind = kind;
   t->attrs[i].name = name;

   return &t->attrs[i];
}

void tree_add_attr_str(tree_t t, ident_t name, ident_t str)
{
   tree_add_attr(t, name, A_STRING)->sval = str;
}

ident_t tree_attr_str(tree_t t, ident_t name)
{
   struct attr *a = tree_find_attr(t, name, A_STRING);
   return a ? a->sval : NULL;
}

void tree_add_attr_int(tree_t t, ident_t name, int n)
{
   tree_add_attr(t, name, A_INT)->ival = n;
}

int tree_attr_int(tree_t t, ident_t name, int def)
{
   struct attr *a = tree_find_attr(t, name, A_INT);
   return a ? a->ival : def;
}

void tree_add_attr_ptr(tree_t t, ident_t name, void *ptr)
{
   tree_add_attr(t, name, A_PTR)->pval = ptr;
}

void *tree_attr_ptr(tree_t t, ident_t name)
{
   struct attr *a = tree_find_attr(t, name, A_PTR);
   return a ? a->pval : NULL;
}

tree_t tree_attr_tree(tree_t t, ident_t name)
{
   struct attr *a = tree_find_attr(t, name, A_TREE);
   return a ? a->tval : NULL;
}

void tree_add_attr_tree(tree_t t, ident_t name, tree_t val)
{
   tree_add_attr(t, name, A_TREE)->tval = val;
}

int64_t assume_int(tree_t t)
{
   switch (tree_kind(t)) {
   case T_LITERAL:
      {
         literal_t l = tree_literal(t);
         assert(l.kind == L_INT);
         return l.i;
      }

   case T_REF:
      {
         tree_t ref = tree_ref(t);
         assert(tree_kind(ref) == T_ENUM_LIT);
         return tree_pos(ref);
      }

   default:
      assert(false);
   }
}

void range_bounds(range_t r, int64_t *low, int64_t *high)
{
   int64_t left  = assume_int(r.left);
   int64_t right = assume_int(r.right);

   if (r.kind == RANGE_TO) {
      *low  = left;
      *high = right;
   }
   else {
      *low  = right;
      *high = left;
   }
}

struct rewrite_ctx {
   tree_t            *cache;
   uint32_t          index;
   uint32_t          generation;
   tree_rewrite_fn_t fn;
   void              *context;
};

static tree_t tree_rewrite_aux(tree_t t, struct rewrite_ctx *ctx);

static void rewrite_a(struct tree_array *a, struct rewrite_ctx *ctx)
{
   for (unsigned i = 0; i < a->count; i++)
      a->items[i] = tree_rewrite_aux(a->items[i], ctx);

   // If an item was rewritten to NULL then delete it
   unsigned n = 0;
   for (unsigned i = 0; i < a->count; i++) {
      if (a->items[i] != NULL)
         a->items[n++] = a->items[i];
   }
   a->count = n;
}

static void rewrite_p(struct param_array *a, struct rewrite_ctx *ctx)
{
   for (unsigned i = 0; i < a->count; i++) {
      switch (a->items[i].kind) {
      case P_RANGE:
         a->items[i].range.left =
            tree_rewrite_aux(a->items[i].range.left, ctx);
         a->items[i].range.right =
            tree_rewrite_aux(a->items[i].range.right, ctx);
         break;

      case P_POS:
      case P_NAMED:
         a->items[i].value = tree_rewrite_aux(a->items[i].value, ctx);
         break;
      }
   }
}

static tree_t tree_rewrite_aux(tree_t t, struct rewrite_ctx *ctx)
{
   if (t == NULL)
      return NULL;

   if (t->generation == ctx->generation) {
      // Already rewritten this tree so return the cached version
      if (ctx->cache[t->index] == NULL)
         fmt_loc(stdout, tree_loc(t));
      return ctx->cache[t->index];
   }

   t->generation = ctx->generation;
   t->index      = ctx->index++;

   if (HAS_GENERICS(t))
      rewrite_a(&t->generics, ctx);
   if (HAS_PORTS(t))
      rewrite_a(&t->ports, ctx);
   if (HAS_DECLS(t))
      rewrite_a(&t->decls, ctx);
   if (HAS_TRIGGERS(t))
      rewrite_a(&t->triggers, ctx);
   if (HAS_STMTS(t))
      rewrite_a(&t->stmts, ctx);
   if (HAS_WAVEFORMS(t))
      rewrite_a(&t->waves, ctx);
   if (HAS_CONDS(t))
      rewrite_a(&t->conds, ctx);
   if (HAS_TARGET(t))
      tree_set_target(t, tree_rewrite_aux(tree_target(t), ctx));
   if (HAS_VALUE(t)) {
      if (tree_has_value(t))
         tree_set_value(t, tree_rewrite_aux(tree_value(t), ctx));
   }
   if (HAS_DELAY(t)) {
      if (tree_has_delay(t))
         tree_set_delay(t, tree_rewrite_aux(tree_delay(t), ctx));
   }
   if (HAS_REJECT(t))
      t->reject = tree_rewrite_aux(t->reject, ctx);
   if (HAS_PARAMS(t))
      rewrite_p(&t->params, ctx);
   if (HAS_RANGE(t)) {
      range_t r = tree_range(t);
      r.left  = tree_rewrite_aux(r.left, ctx);
      r.right = tree_rewrite_aux(r.right, ctx);
      tree_set_range(t, r);
   }
   if (HAS_ASSOCS(t)) {
      for (unsigned i = 0; i < tree_assocs(t); i++) {
         assoc_t *a = &t->assocs[i];
         a->value = tree_rewrite_aux(a->value, ctx);

         switch (a->kind) {
         case A_POS:
         case A_OTHERS:
            break;
         case A_NAMED:
            a->name = tree_rewrite_aux(a->name, ctx);
            break;
         case A_RANGE:
            a->range.left  = tree_rewrite_aux(a->range.left, ctx);
            a->range.right = tree_rewrite_aux(a->range.right, ctx);
            break;
         }
      }
   }

   switch (tree_kind(t)) {
   case T_ASSERT:
      tree_set_severity(t, tree_rewrite_aux(tree_severity(t), ctx));
      tree_set_message(t, tree_rewrite_aux(tree_message(t), ctx));
      break;

   case T_INSTANCE:
      rewrite_p(&t->genmaps, ctx);
      break;

   case T_IF:
      rewrite_a(&t->elses, ctx);
      break;

   default:
      break;
   }

   // Rewrite this tree before we rewrite the type as there may
   // be a circular reference
   ctx->cache[t->index] = (*ctx->fn)(t, ctx->context);

   if (HAS_TYPE(t) && (t->type != NULL)) {
      switch (type_kind(t->type)) {
      case T_INTEGER:
      case T_SUBTYPE:
      case T_PHYSICAL:
      case T_CARRAY:
         for (unsigned i = 0; i < type_dims(t->type); i++) {
            range_t r = type_dim(t->type, i);
            r.left  = tree_rewrite_aux(r.left, ctx);
            r.right = tree_rewrite_aux(r.right, ctx);
            type_change_dim(t->type, i, r);
         }
         break;

      default:
         break;
      }
   }

   return ctx->cache[t->index];
}

tree_t tree_rewrite(tree_t t, tree_rewrite_fn_t fn, void *context)
{
   size_t cache_sz = sizeof(tree_t) * n_trees_alloc;
   tree_t *cache = xmalloc(cache_sz);
   memset(cache, '\0', cache_sz);

   struct rewrite_ctx ctx = {
      .cache      = cache,
      .index      = 0,
      .generation = next_generation++,
      .fn         = fn,
      .context    = context
   };

   tree_t result = tree_rewrite_aux(t, &ctx);
   free(cache);
   return result;
}

struct tree_copy_ctx {
   tree_t   *copied;
   size_t   n_copied;
   unsigned generation;
};

static tree_t tree_copy_aux(tree_t t, struct tree_copy_ctx *ctx);

static void copy_a(struct tree_array *from, struct tree_array *to,
                   struct tree_copy_ctx *ctx)
{
   to->count = to->max = from->count;
   to->items = xmalloc(to->count * sizeof(param_t));

   for (unsigned i = 0; i < from->count; i++)
      to->items[i] = tree_copy_aux(from->items[i], ctx);
}

static void copy_p(struct param_array *from, struct param_array *to,
                   struct tree_copy_ctx *ctx)
{
   to->count = to->max = from->count;
   to->items = xmalloc(to->count * sizeof(param_t));

   for (unsigned i = 0; i < from->count; i++) {
      param_t *fp = &from->items[i];
      param_t *tp = &to->items[i];

      switch ((tp->kind = fp->kind)) {
      case P_POS:
         tp->pos   = fp->pos;
         tp->value = tree_copy_aux(fp->value, ctx);
         break;
      case P_RANGE:
         tp->range.kind  = fp->range.kind;
         tp->range.left  = tree_copy_aux(fp->range.left, ctx);
         tp->range.right = tree_copy_aux(fp->range.right, ctx);
         break;
      case P_NAMED:
         tp->name  = fp->name;
         tp->value = tree_copy_aux(fp->value, ctx);
         break;
      }
   }
}

static tree_t tree_copy_aux(tree_t t, struct tree_copy_ctx *ctx)
{
   if (t == NULL)
      return NULL;

   if (t->generation == ctx->generation) {
      // Already copied this tree
      assert(t->index < ctx->n_copied);
      return ctx->copied[t->index];
   }

   tree_t copy = tree_new(t->kind);

   t->generation = ctx->generation;
   t->index      = (ctx->n_copied)++;
   ctx->copied[t->index] = copy;

   copy->loc        = t->loc;
   if (HAS_IDENT(t))
      copy->ident = t->ident;
   if (HAS_IDENT2(t))
      copy->ident2 = t->ident2;
   if (HAS_PORTS(t))
      copy_a(&t->ports, &copy->ports, ctx);
   if (HAS_GENERICS(t))
      copy_a(&t->generics, &copy->generics, ctx);
   if (HAS_DECLS(t))
      copy_a(&t->decls, &copy->decls, ctx);
   if (HAS_CONDS(t))
      copy_a(&t->conds, &copy->conds, ctx);
   if (HAS_TRIGGERS(t))
      copy_a(&t->triggers, &copy->triggers, ctx);
   if (HAS_STMTS(t))
      copy_a(&t->stmts, &copy->stmts, ctx);
   if (HAS_WAVEFORMS(t))
      copy_a(&t->waves, &copy->waves, ctx);
   if (HAS_TYPE(t))
      copy->type = t->type;
   if (HAS_VALUE(t))
      copy->value = tree_copy_aux(t->value, ctx);
   if (HAS_REJECT(t))
      copy->reject = tree_copy_aux(t->reject, ctx);
   if (HAS_DELAY(t))
      copy->delay = tree_copy_aux(t->delay, ctx);
   if (HAS_TARGET(t))
      copy->target = tree_copy_aux(t->target, ctx);
   if (HAS_REF(t))
      copy->ref = tree_copy_aux(t->ref, ctx);
   if (HAS_CONTEXT(t)) {
      for (unsigned i = 0; i < tree_contexts(t); i++)
         tree_add_context(copy, tree_context(t, i));
   }
   if (HAS_PARAMS(t))
      copy_p(&t->params, &copy->params, ctx);
   if (HAS_RANGE(t)) {
      copy->range.kind  = t->range.kind;
      copy->range.left  = tree_copy_aux(t->range.left, ctx);
      copy->range.right = tree_copy_aux(t->range.right, ctx);
   }
   if (HAS_ASSOCS(t)) {
      for (unsigned i = 0; i < tree_assocs(t); i++) {
         assoc_t a = tree_assoc(t, i);
         switch (a.kind) {
         case A_POS:
         case A_OTHERS:
            break;
         case A_NAMED:
            a.name = tree_copy_aux(a.name, ctx);
            break;
         case A_RANGE:
            a.range.left  = tree_copy_aux(a.range.left, ctx);
            a.range.right = tree_copy_aux(a.range.right, ctx);
            break;
         }

         tree_add_assoc(copy, a);
      }
   }
   if (HAS_CLASS(t))
      copy->class = t->class;

   switch (t->kind) {
   case T_PORT_DECL:
      copy->port_mode = t->port_mode;
      break;

   case T_LITERAL:
      copy->literal = t->literal;
      break;

   case T_ASSERT:
      copy->severity = tree_copy_aux(t->severity, ctx);
      copy->message  = tree_copy_aux(t->message, ctx);
      break;

   case T_ENUM_LIT:
      copy->pos = t->pos;
      break;

   case T_INSTANCE:
      copy_p(&t->genmaps, &copy->genmaps, ctx);
      break;

   case T_IF:
      copy_a(&t->elses, &copy->elses, ctx);
      break;

   default:
      break;
   }

   for (unsigned i = 0; i < t->n_attrs; i++) {
      switch (t->attrs[i].kind) {
      case A_STRING:
         tree_add_attr_str(copy, t->attrs[i].name, t->attrs[i].sval);
         break;

      case A_INT:
         tree_add_attr_int(copy, t->attrs[i].name, t->attrs[i].ival);
         break;

      case A_TREE:
         tree_add_attr_tree(copy, t->attrs[i].name, t->attrs[i].tval);
         break;

      case A_PTR:
         tree_add_attr_ptr(copy, t->attrs[i].name, t->attrs[i].pval);
         break;
      }
   }

   return copy;
}

tree_t tree_copy(tree_t t)
{
   struct tree_copy_ctx ctx = {
      .copied     = xmalloc(sizeof(tree_t) * n_trees_alloc),
      .n_copied   = 0,
      .generation = next_generation++
   };
   tree_t copy = tree_copy_aux(t, &ctx);
   free(ctx.copied);
   return copy;
}

tree_t call_builtin(const char *builtin, type_t type, ...)
{
   struct decl_cache {
      struct decl_cache *next;
      ident_t bname;
      tree_t  decl;
   };

   char name[64];
   snprintf(name, sizeof(name), "NVC.BUILTIN.%s", builtin);
   for (char *p = name; *p != '\0'; p++)
      *p = toupper((uint8_t)*p);

   static struct decl_cache *cache = NULL;

   ident_t bname = ident_new(builtin);
   ident_t name_i = ident_new(name);

   struct decl_cache *it;
   tree_t decl = NULL;
   for (it = cache; it != NULL; it = it->next) {
      if (it->bname == bname) {
         decl = it->decl;
         break;
      }
   }

   if (decl == NULL) {
      decl = tree_new(T_FUNC_DECL);
      tree_set_ident(decl, name_i);
      tree_add_attr_str(decl, ident_new("builtin"), ident_new(builtin));
   }

   struct decl_cache *c = xmalloc(sizeof(struct decl_cache));
   c->next  = cache;
   c->bname = bname;
   c->decl  = decl;

   cache = c;

   tree_t call = tree_new(T_FCALL);
   tree_set_ident(call, name_i);
   tree_set_ref(call, decl);
   if (type != NULL)
      tree_set_type(call, type);

   va_list ap;
   va_start(ap, type);
   tree_t arg;
   while ((arg = va_arg(ap, tree_t))) {
      param_t p = { .kind = P_POS, .value = arg };
      tree_add_param(call, p);
   }
   va_end(ap);

   return call;
}

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
#define FILE_FMT_VER 0x1011
#define MAX_ITEMS    8

#define EXTRA_READ_CHECKS

typedef struct tree_array /* DELME */ {
   uint32_t  count;
   uint32_t  max;
   tree_t   *items;
} tree_array_t;

typedef struct param_array /* DELME */ {
   uint32_t  count;
   uint32_t  max;
   param_t  *items;
} param_array_t;

typedef struct {
   uint32_t  count;
   uint32_t  max;
   assoc_t  *items;
} assoc_array_t;

typedef struct {
   context_t *items;
   uint32_t   count;
} context_set_t;

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

enum {
   I_IDENT     = (1 << 0),
   I_VALUE     = (1 << 1),
   I_SEVERITY  = (1 << 2),
   I_MESSAGE   = (1 << 3),
   I_TARGET    = (1 << 4),
   I_LITERAL   = (1 << 5),
   I_IDENT2    = (1 << 6),
   I_DECLS     = (1 << 7),
   I_STMTS     = (1 << 8),
   I_PORTS     = (1 << 9),
   I_GENERICS  = (1 << 10),
   I_PARAMS    = (1 << 11),
   I_GENMAPS   = (1 << 12),
   I_WAVES     = (1 << 13),
   I_CONDS     = (1 << 14),
   I_TYPE      = (1 << 15),
   I_PORT_MODE = (1 << 16),
   I_DELAY     = (1 << 17),
   I_REJECT    = (1 << 18),
   I_POS       = (1 << 19),
   I_REF       = (1 << 20),
   I_FILE_MODE = (1 << 21),
   I_ASSOCS    = (1 << 22),
   I_CONTEXT   = (1 << 23),
};

typedef union {
   ident_t       ident;
   tree_t        tree;
   literal_t     literal;
   tree_array_t  tree_array;
   param_array_t param_array;
   type_t        type;
   port_mode_t   port_mode;
   uint32_t      uint;
   assoc_array_t assoc_array;
   context_set_t context_set;
} item_t;

typedef uint32_t imask_t;

static const imask_t has_map[T_LAST_TREE_KIND] = {
   // T_ENTITY
   (I_IDENT | I_PORTS | I_GENERICS | I_CONTEXT),

   // T_ARCH
   (I_IDENT | I_IDENT2 | I_DECLS | I_STMTS | I_CONTEXT),

   // T_PORT_DECL
   (I_IDENT | I_VALUE | I_TYPE | I_PORT_MODE),

   // T_FCALL
   (I_IDENT | I_PARAMS | I_TYPE | I_REF),

   // T_LITERAL
   (I_LITERAL | I_TYPE),

   // T_SIGNAL_DECL
   (I_IDENT | I_VALUE | I_TYPE),

   // T_VAR_DECL
   (I_IDENT | I_VALUE | I_TYPE),

   // T_PROCESS
   (I_IDENT | I_DECLS | I_STMTS),

   // T_REF
   (I_IDENT | I_TYPE | I_REF),

   // T_WAIT
   (I_IDENT | I_VALUE | I_DELAY),

   // T_TYPE_DECL
   (I_IDENT | I_VALUE | I_TYPE),

   // T_VAR_ASSIGN
   (I_IDENT | I_VALUE | I_TARGET),

   // T_PACKAGE
   (I_IDENT | I_DECLS | I_CONTEXT),

   // T_SIGNAL_ASSIGN
   (I_IDENT | I_TARGET | I_WAVES | I_REJECT),

   // T_QUALIFIED
   (I_IDENT | I_VALUE | I_TYPE),

   // T_ENUM_LIT
   (I_IDENT | I_TYPE | I_POS),

   // T_CONST_DECL
   (I_IDENT | I_VALUE | I_TYPE),

   // T_FUNC_DECL
   (I_IDENT | I_VALUE | I_PORTS | I_TYPE),

   // T_ELAB
   (I_IDENT | I_DECLS | I_STMTS | I_CONTEXT),

   // T_AGGREGATE
   (I_TYPE | I_ASSOCS),

   // T_ASSERT
   (I_IDENT | I_VALUE | I_SEVERITY | I_MESSAGE),

   // T_ATTR_REF
   (I_IDENT | I_VALUE | I_IDENT2 | I_PARAMS | I_TYPE | I_REF),

   // T_ARRAY_REF
   (I_VALUE | I_PARAMS | I_TYPE),

   // T_ARRAY_SLICE
   (I_VALUE | I_TYPE),

   // T_INSTANCE
   (I_IDENT | I_IDENT2 | I_PARAMS | I_GENMAPS | I_REF),

   // T_IF
   (I_IDENT | I_VALUE | I_STMTS),

   // T_NULL
   (I_IDENT),

   // T_PACK_BODY
   (I_IDENT | I_DECLS | I_CONTEXT),

   // T_FUNC_BODY
   (I_IDENT | I_DECLS | I_STMTS | I_PORTS | I_TYPE),

   // T_RETURN
   (I_IDENT | I_VALUE),

   // T_CASSIGN
   (I_IDENT | I_TARGET | I_CONDS),

   // T_WHILE
   (I_IDENT | I_VALUE | I_STMTS),

   // T_WAVEFORM
   (I_VALUE | I_DELAY),

   // T_ALIAS
   (I_IDENT | I_VALUE | I_TYPE),

   // T_FOR
   (I_IDENT | I_IDENT2 | I_DECLS | I_STMTS),

   // T_ATTR_DECL
   (I_IDENT | I_TYPE),

   // T_ATTR_SPEC
   (I_IDENT | I_VALUE | I_IDENT2),

   // T_PROC_DECL
   (I_IDENT | I_PORTS | I_TYPE),

   // T_PROC_BODY
   (I_IDENT | I_DECLS | I_STMTS | I_PORTS | I_TYPE),

   // T_EXIT
   (I_IDENT | I_VALUE),

   // T_PCALL
   (I_IDENT | I_IDENT2 | I_PARAMS | I_REF),

   // T_CASE
   (I_IDENT | I_VALUE | I_ASSOCS),

   // T_BLOCK
   (I_IDENT | I_DECLS | I_STMTS),

   // T_COND
   (I_VALUE | I_WAVES | I_REJECT),

   // T_CONCAT
   (I_PARAMS | I_TYPE),

   // T_TYPE_CONV
   (I_PARAMS | I_TYPE | I_REF),

   // T_SELECT
   (I_IDENT | I_VALUE | I_ASSOCS),

   // T_COMPONENT
   (I_IDENT | I_PORTS | I_GENERICS),

   // T_IF_GENERATE
   (I_IDENT | I_VALUE | I_DECLS | I_STMTS),

   // T_FOR_GENERATE
   (I_IDENT | I_IDENT2 | I_DECLS | I_STMTS | I_REF),

   // T_FILE_DECL
   (I_IDENT | I_VALUE | I_TYPE | I_FILE_MODE),

   // T_OPEN
   (I_TYPE),
};

#define ITEM_IDENT       (I_IDENT | I_IDENT2)
#define ITEM_TREE        (I_VALUE | I_SEVERITY | I_MESSAGE | I_TARGET \
                          | I_DELAY | I_REJECT | I_REF | I_FILE_MODE)
#define ITEM_LITERAL     (I_LITERAL)
#define ITEM_TREE_ARRAY  (I_DECLS | I_STMTS | I_PORTS | I_GENERICS | I_WAVES \
                          | I_CONDS)
#define ITEM_PARAM_ARRAY (I_PARAMS | I_GENMAPS)
#define ITEM_TYPE        (I_TYPE)
#define ITEM_PORT_MODE   (I_PORT_MODE)
#define ITEM_UINT        (I_POS)
#define ITEM_ASSOC_ARRAY (I_ASSOCS)
#define ITEM_CONTEXT     (I_CONTEXT)

static const char *kind_text_map[T_LAST_TREE_KIND] = {
   "T_ENTITY",       "T_ARCH",          "T_PORT_DECL",  "T_FCALL",
   "T_LITERAL",      "T_SIGNAL_DECL",   "T_VAR_DECL",   "T_PROCESS",
   "T_REF",          "T_WAIT",          "T_TYPE_DECL",  "T_VAR_ASSIGN",
   "T_PACKAGE",      "T_SIGNAL_ASSIGN", "T_QUALIFIED",  "T_ENUM_LIT",
   "T_CONST_DECL",   "T_FUNC_DECL",     "T_ELAB",       "T_AGGREGATE",
   "T_ASSERT",       "T_ATTR_REF",      "T_ARRAY_REF",  "T_ARRAY_SLICE",
   "T_INSTANCE",     "T_IF",            "T_NULL",       "T_PACK_BODY",
   "T_FUNC_BODY",    "T_RETURN",        "T_CASSIGN",    "T_WHILE",
   "T_WAVEFORM",     "T_ALIAS",         "T_FOR",        "T_ATTR_DECL",
   "T_ATTR_SPEC",    "T_PROC_DECL",     "T_PROC_BODY",  "T_EXIT",
   "T_PCALL",        "T_CASE",          "T_BLOCK",      "T_COND",
   "T_CONCAT",       "T_TYPE_CONV",     "T_SELECT",     "T_COMPONENT",
   "T_IF_GENERATE",  "T_FOR_GENERATE",  "T_FILE_DECL",  "T_OPEN",
};

static const char *item_text_map[] = {
   "I_IDENT",    "I_VALUE",     "I_SEVERITY", "I_MESSAGE", "I_TARGET",
   "I_LITERAL",  "I_IDENT2",    "I_DECLS",    "I_STMTS",   "I_PORTS",
   "I_GENERICS", "I_PARAMS",    "I_GENMAPS",  "I_WAVES",   "I_CONDS",
   "I_TYPE",     "I_PORT_MODE", "I_DELAY",    "I_REJECT",  "I_POS",
   "I_REF",      "I_FILE_MODE", "I_ASSOCS",   "I_CONTEXT"
};

struct tree {
   tree_kind_t kind;
   loc_t       loc;
   struct attr *attrs;
   unsigned    n_attrs;
   item_t      items[MAX_ITEMS];

   union {
      range_t           range;     // T_ARRAY_SLICE
      struct tree_array triggers;  // T_WAIT, T_PROCESS
      struct tree_array elses;     // T_IF
      class_t           class;     // T_PORT_DECL
   };

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
    || IS(t, T_PROC_BODY) || IS(t, T_COMPONENT) || IS(t, T_FILE_DECL))
#define IS_EXPR(t)                                                    \
   (IS(t, T_FCALL) || IS(t, T_LITERAL) || IS(t, T_REF)                \
    || IS(t, T_QUALIFIED) || IS(t, T_AGGREGATE) || IS(t, T_ATTR_REF)  \
    || IS(t, T_ARRAY_REF) || IS(t, T_ARRAY_SLICE) || IS(t, T_CONCAT)  \
    || IS(t, T_TYPE_CONV) || IS(t, T_OPEN))
#define IS_STMT(t)                                                    \
   (IS(t, T_PROCESS) || IS(t, T_WAIT) || IS(t, T_VAR_ASSIGN)          \
    || IS(t, T_SIGNAL_ASSIGN) || IS(t, T_ASSERT) || IS(t, T_INSTANCE) \
    || IS(t, T_IF) || IS(t, T_NULL) || IS(t, T_RETURN)                \
    || IS(t, T_CASSIGN) || IS(t, T_WHILE) || IS(t, T_FOR)             \
    || IS(t, T_EXIT) || IS(t, T_PCALL) || IS(t, T_CASE)               \
    || IS(t, T_BLOCK) || IS(t, T_SELECT) || IS(t, T_IF_GENERATE)      \
    || IS(t, T_FOR_GENERATE))
#define HAS_TRIGGERS(t) (IS(t, T_WAIT) || IS(t, T_PROCESS))
#define HAS_RANGE(t)                                                  \
   (IS(t, T_ARRAY_SLICE) || IS(t, T_FOR) || IS(t, T_FOR_GENERATE))
#define HAS_CLASS(t) (IS(t, T_PORT_DECL) || IS(t, T_INSTANCE))

#define TREE_ARRAY_BASE_SZ  16

// Garbage collection
static tree_t *all_trees = NULL;
static size_t max_trees = 128;   // Grows at runtime
static size_t n_trees_alloc = 0;

unsigned next_generation = 1;

static unsigned tree_visit_aux(tree_t t, tree_visit_fn_t fn, void *context,
                               tree_kind_t kind, unsigned generation,
                               bool deep);

static void tree_one_time_init(void)
{
   for (int i = 0; i < T_LAST_TREE_KIND; i++) {
      const int nitems = __builtin_popcount(has_map[i]);
      assert(nitems < MAX_ITEMS);
   }

   printf("sizeof(struct tree) = %d\n", (int)sizeof(struct tree));
}

static item_t *lookup_item(tree_t t, imask_t mask)
{
   assert(t != NULL);
   assert((mask & (mask - 1)) == 0);
   const imask_t has = has_map[t->kind];

   if (unlikely((has & mask) == 0)) {
      int item;
      for (item = 0; (mask & (1 << item)) == 0; item++)
         ;

      assert(item < ARRAY_LEN(item_text_map));
      fatal("tree kind %s does not have item %s",
            kind_text_map[t->kind], item_text_map[item]);
   }

   imask_t m = 1;
   int n = 0;
   while (!(m & mask)) {
      if (has & m)
         n++;
      m <<= 1;
   }

   return &(t->items[n]);
}

static void item_without_type(imask_t mask)
{
   int item;
   for (item = 0; (mask & (1 << item)) == 0; item++)
      ;

   assert(item < ARRAY_LEN(item_text_map));
   fatal("tree item %s does not have a type", item_text_map[item]);
}

static void tree_array_add(tree_array_t *a, tree_t t)
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

static inline tree_t tree_array_nth(tree_array_t *a, unsigned n)
{
   assert(n < a->count);
   return a->items[n];
}
static void assoc_array_add(assoc_array_t *a, assoc_t t)
{
   if (a->max == 0) {
      a->items = xmalloc(sizeof(assoc_t) * TREE_ARRAY_BASE_SZ);
      a->max   = TREE_ARRAY_BASE_SZ;
   }
   else if (a->count == a->max) {
      a->max *= 2;
      a->items = xrealloc(a->items, sizeof(assoc_t) * a->max);
   }

   a->items[a->count++] = t;
}

static inline assoc_t assoc_array_nth(assoc_array_t *a, unsigned n)
{
   assert(n < a->count);
   return a->items[n];
}

static void param_array_add(param_array_t *a, param_t p)
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

static inline param_t param_array_nth(param_array_t *a, unsigned n)
{
   assert(n < a->count);
   return a->items[n];
}

tree_t tree_new(tree_kind_t kind)
{
   static bool done_init = false;
   if (!done_init) {
      tree_one_time_init();
      done_init = true;
   }

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

         const imask_t has = has_map[t->kind];
         const int nitems = __builtin_popcount(has);
         imask_t mask = 1;
         for (int n = 0; n < nitems; mask <<= 1) {
            if (has & mask) {
               if (ITEM_TREE_ARRAY & mask)
                  free(t->items[n].tree_array.items);
               else if (ITEM_PARAM_ARRAY & mask)
                  free(t->items[n].param_array.items);
               else if (ITEM_ASSOC_ARRAY & mask)
                  free(t->items[n].assoc_array.items);
               else if (ITEM_CONTEXT & mask)
                  free(t->items[n].context_set.items);
               n++;
            }
         }

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
   item_t *item = lookup_item(t, I_IDENT);
   assert(item->ident != NULL);
   return item->ident;
}

bool tree_has_ident(tree_t t)
{
   return lookup_item(t, I_IDENT)->ident != NULL;
}

void tree_set_ident(tree_t t, ident_t i)
{
   lookup_item(t, I_IDENT)->ident = i;
}

ident_t tree_ident2(tree_t t)
{
   item_t *item = lookup_item(t, I_IDENT2);
   assert(item->ident != NULL);
   return item->ident;
}

void tree_set_ident2(tree_t t, ident_t i)
{
   lookup_item(t, I_IDENT2)->ident = i;
}

tree_kind_t tree_kind(tree_t t)
{
   assert(t != NULL);
   return t->kind;
}

void tree_change_kind(tree_t t, tree_kind_t kind)
{
   assert(t != NULL);

   const uint32_t old_has = has_map[t->kind];
   const uint32_t new_has = has_map[kind];

   item_t *tmp = xmalloc(sizeof(item_t) * MAX_ITEMS);
   memcpy(tmp, t->items, sizeof(item_t) * MAX_ITEMS);

   const int nitems = __builtin_popcount(new_has);

   int op = 0, np = 0;
   for (uint32_t mask = 1; np < nitems; mask <<= 1) {
      if ((old_has & mask) && (new_has & mask))
         t->items[np++] = tmp[op++];
      else if (old_has & mask)
         ++op;
      else if (new_has & mask)
         memset(&(t->items[np++]), '\0', sizeof(item_t));
   }

   free(tmp);

   t->kind = kind;
}

unsigned tree_ports(tree_t t)
{
   return lookup_item(t, I_PORTS)->tree_array.count;
}

tree_t tree_port(tree_t t, unsigned n)
{
   return tree_array_nth(&(lookup_item(t, I_PORTS)->tree_array), n);
}

void tree_add_port(tree_t t, tree_t d)
{
   assert(IS_DECL(d));
   tree_array_add(&(lookup_item(t, I_PORTS)->tree_array), d);
}

port_mode_t tree_port_mode(tree_t t)
{
   item_t *item = lookup_item(t, I_PORT_MODE);
   assert(item->port_mode != PORT_INVALID);
   return item->port_mode;
}

void tree_set_port_mode(tree_t t, port_mode_t mode)
{
   lookup_item(t, I_PORT_MODE)->port_mode = mode;
}

unsigned tree_generics(tree_t t)
{
   return lookup_item(t, I_GENERICS)->tree_array.count;
}

tree_t tree_generic(tree_t t, unsigned n)
{
   return tree_array_nth(&(lookup_item(t, I_GENERICS)->tree_array), n);
}

void tree_add_generic(tree_t t, tree_t d)
{
   assert(IS_DECL(d));
   tree_array_add(&(lookup_item(t, I_GENERICS)->tree_array), d);
}

type_t tree_type(tree_t t)
{
   item_t *item = lookup_item(t, I_TYPE);
   assert(item->type != NULL);
   return item->type;
}

void tree_set_type(tree_t t, type_t ty)
{
   lookup_item(t, I_TYPE)->type = ty;
}

bool tree_has_type(tree_t t)
{
   return lookup_item(t, I_TYPE)->type != NULL;
}

unsigned tree_params(tree_t t)
{
   return lookup_item(t, I_PARAMS)->param_array.count;
}

param_t tree_param(tree_t t, unsigned n)
{
   return param_array_nth(&(lookup_item(t, I_PARAMS)->param_array), n);
}

void tree_add_param(tree_t t, param_t e)
{
   assert(e.kind == P_RANGE || IS_EXPR(e.value));

   if (e.kind == P_POS)
      e.pos = tree_params(t);

   param_array_add(&(lookup_item(t, I_PARAMS)->param_array), e);
}

unsigned tree_genmaps(tree_t t)
{
   return lookup_item(t, I_GENMAPS)->param_array.count;
}

param_t tree_genmap(tree_t t, unsigned n)
{
   return param_array_nth(&(lookup_item(t, I_GENMAPS)->param_array), n);
}

void tree_add_genmap(tree_t t, param_t e)
{
   assert(e.kind == P_RANGE || IS_EXPR(e.value));

   if (e.kind == P_POS)
      e.pos = tree_genmaps(t);

   param_array_add(&(lookup_item(t, I_GENMAPS)->param_array), e);
}

void tree_set_literal(tree_t t, literal_t lit)
{
   lookup_item(t, I_LITERAL)->literal = lit;
}

literal_t tree_literal(tree_t t)
{
   return lookup_item(t, I_LITERAL)->literal;
}

bool tree_has_value(tree_t t)
{
   return lookup_item(t, I_VALUE)->tree != NULL;
}

tree_t tree_value(tree_t t)
{
   item_t *item = lookup_item(t, I_VALUE);
   assert(item->tree != NULL);
   return item->tree;
}

void tree_set_value(tree_t t, tree_t v)
{
   assert(v == NULL || IS_EXPR(v));
   lookup_item(t, I_VALUE)->tree = v;
}

unsigned tree_decls(tree_t t)
{
   return lookup_item(t, I_DECLS)->tree_array.count;
}

tree_t tree_decl(tree_t t, unsigned n)
{
   return tree_array_nth(&(lookup_item(t, I_DECLS)->tree_array), n);
}

void tree_add_decl(tree_t t, tree_t d)
{
   assert(IS_DECL(d));
   tree_array_add(&(lookup_item(t, I_DECLS)->tree_array), d);
}

unsigned tree_stmts(tree_t t)
{
   return lookup_item(t, I_STMTS)->tree_array.count;
}

tree_t tree_stmt(tree_t t, unsigned n)
{
   return tree_array_nth(&(lookup_item(t, I_STMTS)->tree_array), n);
}

void tree_add_stmt(tree_t t, tree_t s)
{
   assert(IS_STMT(s));
   tree_array_add(&(lookup_item(t, I_STMTS)->tree_array), s);
}

unsigned tree_waveforms(tree_t t)
{
   return lookup_item(t, I_WAVES)->tree_array.count;
}

tree_t tree_waveform(tree_t t, unsigned n)
{
   return tree_array_nth(&(lookup_item(t, I_WAVES)->tree_array), n);
}

void tree_add_waveform(tree_t t, tree_t w)
{
   assert(IS(w, T_WAVEFORM));
   tree_array_add(&(lookup_item(t, I_WAVES)->tree_array), w);
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
   return lookup_item(t, I_CONDS)->tree_array.count;
}

tree_t tree_cond(tree_t t, unsigned n)
{
   return tree_array_nth(&(lookup_item(t, I_CONDS)->tree_array), n);
}

void tree_add_cond(tree_t t, tree_t c)
{
   assert(IS(c, T_COND));
   tree_array_add(&(lookup_item(t, I_CONDS)->tree_array), c);
}

bool tree_has_delay(tree_t t)
{
   return lookup_item(t, I_DELAY)->tree != NULL;
}

tree_t tree_delay(tree_t t)
{
   item_t *item = lookup_item(t, I_DELAY);
   assert(item->tree != NULL);
   return item->tree;
}

void tree_set_delay(tree_t t, tree_t d)
{
   assert(IS_EXPR(d));
   lookup_item(t, I_DELAY)->tree = d;
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
   item_t *item = lookup_item(t, I_TARGET);
   assert(item->tree != NULL);
   return item->tree;
}

void tree_set_target(tree_t t, tree_t lhs)
{
   lookup_item(t, I_TARGET)->tree = lhs;
}

tree_t tree_ref(tree_t t)
{
   item_t *item = lookup_item(t, I_REF);
   assert(item->tree != NULL);
   return item->tree;
}

void tree_set_ref(tree_t t, tree_t decl)
{
   assert(IS_DECL(decl) || IS(decl, T_ENUM_LIT) || IS_TOP_LEVEL(decl));
   lookup_item(t, I_REF)->tree = decl;
}

unsigned tree_contexts(tree_t t)
{
   return lookup_item(t, I_CONTEXT)->context_set.count;
}

context_t tree_context(tree_t t, unsigned n)
{
   item_t *item = lookup_item(t, I_CONTEXT);
   assert(n < item->context_set.count);
   return item->context_set.items[n];
}

void tree_add_context(tree_t t, context_t ctx)
{
   item_t *item = lookup_item(t, I_CONTEXT);
   assert(item->context_set.count < MAX_CONTEXTS);

   for (unsigned i = 0; i < item->context_set.count; i++) {
      if (item->context_set.items[i].name == ctx.name)
         return;
   }

   if (item->context_set.count == 0)
      item->context_set.items = xmalloc(sizeof(context_t) * MAX_CONTEXTS);

   item->context_set.items[item->context_set.count++] = ctx;
}

unsigned tree_assocs(tree_t t)
{
   return lookup_item(t, I_ASSOCS)->assoc_array.count;
}

assoc_t tree_assoc(tree_t t, unsigned n)
{
   return assoc_array_nth(&(lookup_item(t, I_ASSOCS)->assoc_array), n);
}

void tree_change_assoc(tree_t t, unsigned i, assoc_t a)
{
   assert(i < tree_assocs(t));
   lookup_item(t, I_ASSOCS)->assoc_array.items[i] = a;
}

void tree_add_assoc(tree_t t, assoc_t a)
{
   assoc_array_t *array = &(lookup_item(t, I_ASSOCS)->assoc_array);

   if (a.kind == A_POS) {
      unsigned pos = 0;
      for (unsigned i = 0; i < array->count; i++) {
         if (array->items[i].kind == A_POS)
            pos++;
      }

      a.pos = pos;
   }

   assoc_array_add(array, a);
}

tree_t tree_severity(tree_t t)
{
   item_t *item = lookup_item(t, I_SEVERITY);
   assert(item->tree != NULL);
   return item->tree;
}

void tree_set_severity(tree_t t, tree_t s)
{
   assert(IS_EXPR(s));
   lookup_item(t, I_SEVERITY)->tree = s;
}

tree_t tree_message(tree_t t)
{
   item_t *item = lookup_item(t, I_MESSAGE);
   assert(item->tree != NULL);
   return item->tree;
}

void tree_set_message(tree_t t, tree_t m)
{
   assert(IS_EXPR(m));
   lookup_item(t, I_MESSAGE)->tree = m;
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
   return lookup_item(t, I_POS)->uint;
}

void tree_set_pos(tree_t t, unsigned pos)
{
   lookup_item(t, I_POS)->uint = pos;
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
   item_t *item = lookup_item(t, I_REJECT);
   assert(item->tree != NULL);
   return item->tree;
}

void tree_set_reject(tree_t t, tree_t r)
{
   assert(IS_EXPR(r));
   lookup_item(t, I_REJECT)->tree = r;
}

tree_t tree_file_mode(tree_t t)
{
   return lookup_item(t, I_FILE_MODE)->tree;
}

void tree_set_file_mode(tree_t t, tree_t m)
{
   lookup_item(t, I_FILE_MODE)->tree = m;
}

uint32_t tree_index(tree_t t)
{
   assert(t != NULL);
   assert(t->index != UINT32_MAX);

   return t->index;
}

static unsigned tree_visit_a(tree_array_t *a,
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

static unsigned tree_visit_s(assoc_array_t *a,
                             tree_visit_fn_t fn, void *context,
                             tree_kind_t kind, unsigned generation,
                             bool deep)
{
   unsigned n = 0;
   for (unsigned i = 0; i < a->count; i++) {
      switch (a->items[i].kind) {
         case A_NAMED:
            n += tree_visit_aux(a->items[i].name, fn, context,
                                kind, generation, deep);
            break;
         case A_RANGE:
            n += tree_visit_aux(a->items[i].range.left, fn, context,
                                kind, generation, deep);
            n += tree_visit_aux(a->items[i].range.right, fn, context,
                                kind, generation, deep);
            break;
         default:
            break;
         }

         n += tree_visit_aux(a->items[i].value, fn, context,
                             kind, generation, deep);
   }

   return n;
}

static unsigned tree_visit_p(param_array_t *a,
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
   case T_REAL:
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
      n += tree_visit_type(type_base(type), fn, context, kind,
                           generation, deep);
      break;

   case T_CARRAY:
   case T_UARRAY:
      n += tree_visit_type(type_elem(type), fn, context, kind,
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

   const imask_t deep_mask = I_TYPE | I_REF;

   const imask_t has = has_map[t->kind];
   const int nitems = __builtin_popcount(has);
   imask_t mask = 1;
   for (int i = 0; i < nitems; mask <<= 1) {
      if (has & mask & ~(deep ? 0 : deep_mask)) {
         if (ITEM_IDENT & mask)
            ;
         else if (ITEM_TREE & mask)
            n += tree_visit_aux(t->items[i].tree, fn, context,
                                kind, generation, deep);
         else if (ITEM_LITERAL & mask)
            ;
         else if (ITEM_TREE_ARRAY & mask)
            n += tree_visit_a(&(t->items[i].tree_array), fn, context,
                              kind, generation, deep);
         else if (ITEM_PARAM_ARRAY & mask)
            n += tree_visit_p(&(t->items[i].param_array), fn, context,
                              kind, generation, deep);
         else if (ITEM_TYPE & mask)
            n += tree_visit_type(t->items[i].type, fn, context, kind,
                                 generation, deep);
         else if (ITEM_PORT_MODE & mask)
            ;
         else if (ITEM_UINT & mask)
            ;
         else if (ITEM_ASSOC_ARRAY & mask)
            n += tree_visit_s(&(t->items[i].assoc_array), fn, context, kind,
                              generation, deep);
         else if (ITEM_CONTEXT & mask)
            ;
         else
            item_without_type(mask);
      }

      if (has & mask)
         i++;
   }

   if (HAS_TRIGGERS(t))
      n += tree_visit_a(&t->triggers, fn, context, kind, generation, deep);
   if (HAS_RANGE(t)) {
      n += tree_visit_aux(t->range.left, fn, context, kind,
                          generation, deep);
      n += tree_visit_aux(t->range.right, fn, context, kind,
                          generation, deep);
   }

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

static void write_s(assoc_array_t *a, tree_wr_ctx_t ctx)
{
   write_u16(a->count, ctx->file);

   for (unsigned i = 0; i < a->count; i++) {
      write_u8(a->items[i].kind, ctx->file);
      tree_write(a->items[i].value, ctx);

      switch (a->items[i].kind) {
      case A_POS:
         write_u32(a->items[i].pos, ctx->file);
         break;
      case A_NAMED:
         tree_write(a->items[i].name, ctx);
         break;
      case A_RANGE:
         write_u16(a->items[i].range.kind, ctx->file);
         tree_write(a->items[i].range.left, ctx);
         tree_write(a->items[i].range.right, ctx);
         break;
      case A_OTHERS:
         break;
      default:
         assert(false);
      }
   }
}

static void read_a(struct tree_array *a, tree_rd_ctx_t ctx)
{
   a->count = a->max = read_u32(ctx->file);
   a->items = xmalloc(a->count * sizeof(tree_t));
   for (unsigned i = 0; i < a->count; i++)
      a->items[i] = tree_read(ctx);
}

static void read_s(assoc_array_t *a, tree_rd_ctx_t ctx)
{
   a->count = a->max = read_u16(ctx->file);
   a->items = xmalloc(sizeof(assoc_t) * a->count);

   for (unsigned i = 0; i < a->count; i++) {
      a->items[i].kind  = read_u8(ctx->file);
      a->items[i].value = tree_read(ctx);

      switch (a->items[i].kind) {
      case A_POS:
         a->items[i].pos = read_u32(ctx->file);
         break;
      case A_NAMED:
         a->items[i].name = tree_read(ctx);
         break;
      case A_RANGE:
         a->items[i].range.kind  = read_u16(ctx->file);
         a->items[i].range.left  = tree_read(ctx);
         a->items[i].range.right = tree_read(ctx);
         break;
      case A_OTHERS:
         break;
      default:
         assert(false);
      }
   }
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
#ifdef EXTRA_READ_CHECKS
   write_u16(0xf11f, ctx->file);
#endif  // EXTRA_READ_CHECKS

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

   const uint32_t has = has_map[t->kind];
   const int nitems = __builtin_popcount(has);
   uint32_t mask = 1;
   for (int n = 0; n < nitems; mask <<= 1) {
      if (has & mask) {
         if (ITEM_IDENT & mask)
            ident_write(t->items[n].ident, ctx->ident_ctx);
         else if (ITEM_TREE & mask)
            tree_write(t->items[n].tree, ctx);
         else if (ITEM_LITERAL & mask) {
            literal_t *l = &(t->items[n].literal);
            write_u16(l->kind, ctx->file);
            switch (l->kind) {
            case L_REAL:
            case L_INT:
               write_i64(l->i, ctx->file);
               break;
            default:
               assert(false);
            }
         }
         else if (ITEM_TREE_ARRAY & mask)
            write_a(&(t->items[n].tree_array), ctx);
         else if (ITEM_PARAM_ARRAY & mask)
            write_p(&(t->items[n].param_array), ctx);
         else if (ITEM_TYPE & mask)
            type_write(t->items[n].type, ctx->type_ctx);
         else if (ITEM_PORT_MODE & mask)
            write_u8(t->items[n].port_mode, ctx->file);
         else if (ITEM_UINT & mask)
            write_u32(t->items[n].uint, ctx->file);
         else if (ITEM_ASSOC_ARRAY & mask)
            write_s(&(t->items[n].assoc_array), ctx);
         else if (ITEM_CONTEXT & mask) {
            write_u16(t->items[n].context_set.count, ctx->file);
            for (unsigned i = 0; i < t->items[n].context_set.count; i++) {
               context_t *c = &(t->items[n].context_set.items[i]);
               ident_write(c->name, ctx->ident_ctx);
               write_loc(&(c->loc), ctx);
            }
         }
         else
            item_without_type(mask);
         n++;
      }
   }

   if (HAS_TRIGGERS(t))
      write_a(&t->triggers, ctx);
   if (HAS_RANGE(t)) {
      write_u16(t->range.kind, ctx->file);
      tree_write(t->range.left, ctx);
      tree_write(t->range.right, ctx);
   }
   if (HAS_CLASS(t))
      write_u16(t->class, ctx->file);

   switch (t->kind) {
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
#ifdef EXTRA_READ_CHECKS
   uint16_t start = read_u16(ctx->file);
   if (start != 0xf11f)
      fatal("bad tree start marker %x", start);
#endif  // EXTRA_READ_CHECKS

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

   const uint32_t has = has_map[t->kind];
   const int nitems = __builtin_popcount(has);
   uint32_t mask = 1;
   for (int n = 0; n < nitems; mask <<= 1) {
      if (has & mask) {
         if (ITEM_IDENT & mask)
            t->items[n].ident = ident_read(ctx->ident_ctx);
         else if (ITEM_TREE & mask)
            t->items[n].tree = tree_read(ctx);
         else if (ITEM_LITERAL & mask) {
            literal_t *l = &(t->items[n].literal);
            l->kind = read_u16(ctx->file);
            switch (l->kind) {
            case L_INT:
            case L_REAL:
               l->i = read_i64(ctx->file);
               break;
            default:
               assert(false);
            }
         }
         else if (ITEM_TREE_ARRAY & mask)
            read_a(&(t->items[n].tree_array), ctx);
         else if (ITEM_PARAM_ARRAY & mask)
            read_p(&(t->items[n].param_array), ctx);
         else if (ITEM_TYPE & mask)
            t->items[n].type = type_read(ctx->type_ctx);
         else if (ITEM_PORT_MODE & mask)
            t->items[n].port_mode = read_u8(ctx->file);
         else if (ITEM_UINT & mask)
            t->items[n].uint = read_u32(ctx->file);
         else if (ITEM_ASSOC_ARRAY & mask)
            read_s(&(t->items[n].assoc_array), ctx);
         else if (ITEM_CONTEXT & mask) {
            t->items[n].context_set.count = read_u16(ctx->file);
            t->items[n].context_set.items =
               xmalloc(sizeof(context_t) * MAX_CONTEXTS);

            for (unsigned i = 0; i < t->items[n].context_set.count; i++) {
               context_t *c = &(t->items[n].context_set.items[i]);
               c->name = ident_read(ctx->ident_ctx);
               c->loc  = read_loc(ctx);
            }
         }
         else
            item_without_type(mask);
         n++;
      }
   }

   if (HAS_TRIGGERS(t))
      read_a(&t->triggers, ctx);
   if (HAS_RANGE(t)) {
      t->range.kind  = read_u16(ctx->file);
      t->range.left  = tree_read(ctx);
      t->range.right = tree_read(ctx);
   }
   if (HAS_CLASS(t))
      t->class = read_u16(ctx->file);

   switch (t->kind) {
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
   uint16_t term = read_u16(ctx->file);
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
   assert(name != NULL);

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
   for (size_t i = 0; i < a->count; i++)
      a->items[i] = tree_rewrite_aux(a->items[i], ctx);

   // If an item was rewritten to NULL then delete it
   size_t n = 0;
   for (size_t i = 0; i < a->count; i++) {
      if (a->items[i] != NULL)
         a->items[n++] = a->items[i];
   }
   a->count = n;
}

static void rewrite_s(assoc_array_t *a, struct rewrite_ctx *ctx)
{
   for (unsigned i = 0; i < a->count; i++) {
      assoc_t *s = &a->items[i];
      s->value = tree_rewrite_aux(s->value, ctx);

      switch (s->kind) {
      case A_POS:
      case A_OTHERS:
         break;
      case A_NAMED:
         s->name = tree_rewrite_aux(s->name, ctx);
         break;
      case A_RANGE:
         s->range.left  = tree_rewrite_aux(s->range.left, ctx);
         s->range.right = tree_rewrite_aux(s->range.right, ctx);
         break;
      }
   }
}

static void rewrite_p(struct param_array *a, struct rewrite_ctx *ctx)
{
   for (size_t i = 0; i < a->count; i++) {
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
      return ctx->cache[t->index];
   }

   t->generation = ctx->generation;
   t->index      = ctx->index++;

   const imask_t skip_mask = I_REF;

   const imask_t has = has_map[t->kind];
   const int nitems = __builtin_popcount(has);
   imask_t mask = 1;
   for (int n = 0; n < nitems; mask <<= 1) {
      if (has & mask & ~skip_mask) {
         if (ITEM_IDENT & mask)
            ;
         else if (ITEM_TREE & mask)
            t->items[n].tree = tree_rewrite_aux(t->items[n].tree, ctx);
         else if (ITEM_LITERAL & mask)
            ;
         else if (ITEM_TREE_ARRAY & mask)
            rewrite_a(&(t->items[n].tree_array), ctx);
         else if (ITEM_PARAM_ARRAY & mask)
            rewrite_p(&(t->items[n].param_array), ctx);
         else if (ITEM_TYPE & mask)
            ;
         else if (ITEM_PORT_MODE & mask)
            ;
         else if (ITEM_UINT & mask)
            ;
         else if (ITEM_ASSOC_ARRAY & mask)
            rewrite_s(&(t->items[n].assoc_array), ctx);
         else if (ITEM_CONTEXT & mask)
            ;
         else
            item_without_type(mask);
      }

      if (has & mask)
         n++;
   }

   if (HAS_TRIGGERS(t))
      rewrite_a(&t->triggers, ctx);
   if (HAS_RANGE(t)) {
      range_t r = tree_range(t);
      r.left  = tree_rewrite_aux(r.left, ctx);
      r.right = tree_rewrite_aux(r.right, ctx);
      tree_set_range(t, r);
   }

   switch (tree_kind(t)) {
   case T_ASSERT:
      tree_set_severity(t, tree_rewrite_aux(tree_severity(t), ctx));
      tree_set_message(t, tree_rewrite_aux(tree_message(t), ctx));
      break;

   case T_IF:
      rewrite_a(&t->elses, ctx);
      break;

   default:
      break;
   }

   // Deleting the target deletes the statement
   if ((has & I_TARGET) && (lookup_item(t, I_TARGET)->tree == NULL))
      return NULL;

   // Rewrite this tree before we rewrite the type as there may
   // be a circular reference
   ctx->cache[t->index] = (*ctx->fn)(t, ctx->context);

   type_t type;
   if ((has & I_TYPE) && (type = lookup_item(t, I_TYPE)->type)) {
      switch (type_kind(type)) {
      case T_INTEGER:
      case T_REAL:
      case T_SUBTYPE:
      case T_PHYSICAL:
      case T_CARRAY:
         for (unsigned i = 0; i < type_dims(type); i++) {
            range_t r = type_dim(type, i);
            r.left  = tree_rewrite_aux(r.left, ctx);
            r.right = tree_rewrite_aux(r.right, ctx);
            type_change_dim(type, i, r);
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

   for (size_t i = 0; i < from->count; i++)
      to->items[i] = tree_copy_aux(from->items[i], ctx);
}

static void copy_s(assoc_array_t *from, assoc_array_t *to,
                   struct tree_copy_ctx *ctx)
{
   to->count = to->max = from->count;
   to->items = xmalloc(to->count * sizeof(assoc_t));

   for (unsigned i = 0; i < from->count; i++) {
      to->items[i].kind = from->items[i].kind;
      to->items[i].value = from->items[i].value;

      switch (from->items[i].kind) {
      case A_POS:
      case A_OTHERS:
         break;
      case A_NAMED:
         to->items[i].name = tree_copy_aux(from->items[i].name, ctx);
         break;
      case A_RANGE:
         to->items[i].range.left =
            tree_copy_aux(from->items[i].range.left, ctx);
         to->items[i].range.right =
            tree_copy_aux(from->items[i].range.right, ctx);
         break;
      }
   }
}

static void copy_p(struct param_array *from, struct param_array *to,
                   struct tree_copy_ctx *ctx)
{
   to->count = to->max = from->count;
   to->items = xmalloc(to->count * sizeof(param_t));

   for (size_t i = 0; i < from->count; i++) {
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

   copy->loc = t->loc;

   const imask_t has = has_map[t->kind];
   const int nitems = __builtin_popcount(has);
   imask_t mask = 1;
   for (int n = 0; n < nitems; mask <<= 1) {
      if (has & mask) {
         if (ITEM_IDENT & mask)
            copy->items[n].ident = t->items[n].ident;
         else if (ITEM_TREE & mask)
            copy->items[n].tree = tree_copy_aux(t->items[n].tree, ctx);
         else if (ITEM_LITERAL & mask)
            copy->items[n].literal = t->items[n].literal;
         else if (ITEM_TREE_ARRAY & mask)
            copy_a(&(t->items[n].tree_array),
                   &(copy->items[n].tree_array), ctx);
         else if (ITEM_PARAM_ARRAY & mask)
            copy_p(&(t->items[n].param_array),
                   &(copy->items[n].param_array), ctx);
         else if (ITEM_TYPE & mask)
            copy->items[n].type = t->items[n].type;
         else if (ITEM_PORT_MODE & mask)
            copy->items[n].port_mode = t->items[n].port_mode;
         else if (ITEM_UINT & mask)
            copy->items[n].uint = t->items[n].uint;
         else if (ITEM_ASSOC_ARRAY & mask)
            copy_s(&(t->items[n].assoc_array),
                   &(copy->items[n].assoc_array), ctx);
         else if (ITEM_CONTEXT & mask) {
            copy->items[n].context_set.count = t->items[n].context_set.count;
            copy->items[n].context_set.items =
               xmalloc(MAX_CONTEXTS * sizeof(context_t));
            memcpy(copy->items[n].context_set.items,
                   t->items[n].context_set.items,
                   t->items[n].context_set.count * sizeof(context_t));
         }
         else
            item_without_type(mask);
         n++;
      }
   }

   if (HAS_TRIGGERS(t))
      copy_a(&t->triggers, &copy->triggers, ctx);
   if (HAS_RANGE(t)) {
      copy->range.kind  = t->range.kind;
      copy->range.left  = tree_copy_aux(t->range.left, ctx);
      copy->range.right = tree_copy_aux(t->range.right, ctx);
   }
   if (HAS_CLASS(t))
      copy->class = t->class;

   switch (t->kind) {
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
      param_t p = { .value = arg, { .pos = 0 }, .kind = P_POS };
      tree_add_param(call, p);
   }
   va_end(ap);

   return call;
}

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

#include "tree.h"
#include "util.h"
#include "array.h"
#include "object.h"

#include <assert.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

#define MAX_CONTEXTS 16
#define MAX_FILES    512

//#define EXTRA_READ_CHECKS

DEFINE_ARRAY(tree);
DEFINE_ARRAY(netid);

typedef enum {
   A_STRING, A_INT, A_PTR, A_TREE
} attr_kind_t;

typedef struct {
   attr_kind_t kind;
   ident_t     name;
   union {
      ident_t sval;
      int     ival;
      void    *pval;
      tree_t  tval;
   };
} attr_t;

typedef struct {
   uint16_t  alloc;
   uint16_t  num;
   attr_t   *table;
} attr_tab_t;

enum {
   I_IDENT     = (1 << 0),
   I_VALUE     = (1 << 1),
   I_SEVERITY  = (1 << 2),
   I_MESSAGE   = (1 << 3),
   I_TARGET    = (1 << 4),
   I_IVAL      = (1 << 5),
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
   I_SUBKIND   = (1 << 16),
   I_DELAY     = (1 << 17),
   I_REJECT    = (1 << 18),
   I_POS       = (1 << 19),
   I_REF       = (1 << 20),
   I_FILE_MODE = (1 << 21),
   I_ASSOCS    = (1 << 22),
   I_CONTEXT   = (1 << 23),
   I_TRIGGERS  = (1 << 24),
   I_ELSES     = (1 << 25),
   I_CLASS     = (1 << 26),
   I_RANGE     = (1 << 27),
   I_NAME      = (1 << 28),
   I_NETS      = (1 << 29),
   I_DVAL      = (1 << 30),
};

typedef union {
   ident_t        ident;
   tree_t         tree;
   tree_array_t   tree_array;
   type_t         type;
   unsigned       subkind;
   int64_t        ival;
   double         dval;
   range_t       *range;
   netid_array_t  netid_array;
} item_t;

typedef uint32_t imask_t;

static const imask_t has_map[T_LAST_TREE_KIND] = {
   // T_ENTITY
   (I_IDENT | I_PORTS | I_GENERICS | I_CONTEXT | I_DECLS),

   // T_ARCH
   (I_IDENT | I_IDENT2 | I_DECLS | I_STMTS | I_CONTEXT | I_REF),

   // T_PORT_DECL
   (I_IDENT | I_VALUE | I_TYPE | I_SUBKIND | I_CLASS),

   // T_FCALL
   (I_IDENT | I_PARAMS | I_TYPE | I_REF),

   // T_LITERAL
   (I_SUBKIND | I_TYPE | I_IVAL | I_DVAL),

   // T_SIGNAL_DECL
   (I_IDENT | I_VALUE | I_TYPE | I_NETS),

   // T_VAR_DECL
   (I_IDENT | I_VALUE | I_TYPE),

   // T_PROCESS
   (I_IDENT | I_DECLS | I_STMTS | I_TRIGGERS),

   // T_REF
   (I_IDENT | I_TYPE | I_REF),

   // T_WAIT
   (I_IDENT | I_VALUE | I_DELAY | I_TRIGGERS),

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
   (I_NAME | I_VALUE | I_IDENT | I_PARAMS | I_TYPE | I_REF),

   // T_ARRAY_REF
   (I_VALUE | I_PARAMS | I_TYPE),

   // T_ARRAY_SLICE
   (I_VALUE | I_TYPE | I_RANGE),

   // T_INSTANCE
   (I_IDENT | I_IDENT2 | I_PARAMS | I_GENMAPS | I_REF | I_CLASS),

   // T_IF
   (I_IDENT | I_VALUE | I_STMTS | I_ELSES),

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
   (I_IDENT | I_IDENT2 | I_DECLS | I_STMTS | I_RANGE),

   // T_ATTR_DECL
   (I_IDENT | I_TYPE),

   // T_ATTR_SPEC
   (I_IDENT | I_VALUE | I_IDENT2 | I_CLASS),

   // T_PROC_DECL
   (I_IDENT | I_PORTS | I_TYPE),

   // T_PROC_BODY
   (I_IDENT | I_DECLS | I_STMTS | I_PORTS | I_TYPE),

   // T_EXIT
   (I_IDENT | I_VALUE | I_IDENT2),

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
   (I_IDENT | I_IDENT2 | I_DECLS | I_STMTS | I_REF | I_RANGE),

   // T_FILE_DECL
   (I_IDENT | I_VALUE | I_TYPE | I_FILE_MODE),

   // T_OPEN
   (I_TYPE),

   // T_FIELD_DECL
   (I_IDENT | I_TYPE),

   // T_RECORD_REF
   (I_IDENT | I_VALUE | I_TYPE),

   // T_ALL
   (I_VALUE | I_TYPE),

   // T_NEW
   (I_VALUE | I_TYPE),

   // T_CASSERT
   (I_IDENT | I_VALUE | I_SEVERITY | I_MESSAGE),

   // T_CPCALL
   (I_IDENT | I_IDENT2 | I_PARAMS | I_REF),

   // T_UNIT_DECL
   (I_IDENT | I_VALUE | I_TYPE),

   // T_NEXT
   (I_IDENT | I_VALUE | I_IDENT2),

   // T_GENVAR
   (I_IDENT | I_TYPE),

   // T_PARAM
   (I_VALUE | I_POS | I_SUBKIND | I_NAME),

   // T_ASSOC
   (I_VALUE | I_POS | I_NAME | I_RANGE | I_SUBKIND),

   // T_CONTEXT
   (I_IDENT | I_IDENT2),

   // T_HIER
   (I_IDENT | I_SUBKIND | I_IDENT2),
};

#define ITEM_IDENT       (I_IDENT | I_IDENT2)
#define ITEM_TREE        (I_VALUE | I_SEVERITY | I_MESSAGE | I_TARGET \
                          | I_DELAY | I_REJECT | I_REF | I_FILE_MODE  \
                          | I_NAME)
#define ITEM_TREE_ARRAY  (I_DECLS | I_STMTS | I_PORTS | I_GENERICS | I_WAVES \
                          | I_CONDS | I_TRIGGERS | I_ELSES | I_PARAMS  \
                          | I_GENMAPS | I_ASSOCS | I_CONTEXT)
#define ITEM_TYPE        (I_TYPE)
#define ITEM_INT64       (I_POS | I_SUBKIND | I_CLASS | I_IVAL)
#define ITEM_RANGE       (I_RANGE)
#define ITEM_NETID_ARRAY (I_NETS)
#define ITEM_DOUBLE      (I_DVAL)

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
   "T_FIELD_DECL",   "T_RECORD_REF",    "T_ALL",        "T_NEW",
   "T_CASSERT",      "T_CPCALL",        "T_UNIT_DECL",  "T_NEXT",
   "T_GENVAR",       "T_PARAM",         "T_ASSOC",      "T_CONTEXT",
   "T_HIER"
};

static const char *item_text_map[] = {
   "I_IDENT",    "I_VALUE",     "I_SEVERITY", "I_MESSAGE", "I_TARGET",
   "I_LITERAL",  "I_IDENT2",    "I_DECLS",    "I_STMTS",   "I_PORTS",
   "I_GENERICS", "I_PARAMS",    "I_GENMAPS",  "I_WAVES",   "I_CONDS",
   "I_TYPE",     "I_SUBKIND",   "I_DELAY",    "I_REJECT",  "I_POS",
   "I_REF",      "I_FILE_MODE", "I_ASSOCS",   "I_CONTEXT", "I_TRIGGERS",
   "I_ELSES",    "I_CLASS",     "I_RANGE",    "I_NAME",    "I_NETS",
};

static const tree_kind_t change_allowed[][2] = {
   { T_REF,       T_FCALL      },
   { T_ARRAY_REF, T_FCALL      },
   { T_FCALL,     T_ARRAY_REF  },
   { T_FCALL,     T_PCALL      },
   { T_FCALL,     T_TYPE_CONV  },
   { T_REF,       T_RECORD_REF },
};

struct tree {
   tree_kind_t kind;
   loc_t       loc;
   attr_tab_t  attrs;
   uint32_t    generation;
   uint32_t    index;
   item_t      items[0];
};

struct tree_wr_ctx {
   fbuf_t         *file;
   type_wr_ctx_t   type_ctx;
   ident_wr_ctx_t  ident_ctx;
   unsigned        generation;
   unsigned        n_trees;
   const char     *file_names[MAX_FILES];
};

struct tree_rd_ctx {
   fbuf_t         *file;
   type_rd_ctx_t   type_ctx;
   ident_rd_ctx_t  ident_ctx;
   unsigned        n_trees;
   tree_t         *store;
   unsigned        store_sz;
   char           *db_fname;
   const char     *file_names[MAX_FILES];
};

static const tree_kind_t stmt_kinds[] = {
   T_PROCESS, T_WAIT,        T_VAR_ASSIGN,   T_SIGNAL_ASSIGN,
   T_ASSERT,  T_INSTANCE,    T_IF,           T_NULL,
   T_RETURN,  T_CASSIGN,     T_WHILE,        T_FOR,
   T_EXIT,    T_PCALL,       T_CASE,         T_BLOCK,
   T_SELECT,  T_IF_GENERATE, T_FOR_GENERATE, T_CPCALL,
   T_CASSERT, T_NEXT
};

static tree_kind_t expr_kinds[] = {
   T_FCALL,     T_LITERAL,   T_REF,       T_QUALIFIED,
   T_AGGREGATE, T_ATTR_REF,  T_ARRAY_REF, T_ARRAY_SLICE,
   T_CONCAT,    T_TYPE_CONV, T_OPEN,      T_RECORD_REF,
   T_ALL,       T_NEW
};

static tree_kind_t decl_kinds[] = {
   T_PORT_DECL,  T_SIGNAL_DECL, T_VAR_DECL,   T_TYPE_DECL,
   T_CONST_DECL, T_FUNC_DECL,   T_FUNC_BODY,  T_ALIAS,
   T_ATTR_DECL,  T_ATTR_SPEC,   T_PROC_DECL,  T_PROC_BODY,
   T_COMPONENT,  T_FILE_DECL,   T_FIELD_DECL, T_UNIT_DECL,
   T_GENVAR,     T_HIER,
};

static tree_kind_t top_level_kinds[] = {
   T_ARCH, T_ENTITY, T_PACKAGE,  T_ELAB, T_PACK_BODY
};

// Garbage collection
static tree_t *all_trees = NULL;
static size_t max_trees = 128;   // Grows at runtime
static size_t n_trees_alloc = 0;

static uint32_t format_digest;
static int      item_lookup[T_LAST_TREE_KIND][32];
static size_t   object_size[T_LAST_TREE_KIND];
static int      object_nitems[T_LAST_TREE_KIND];

unsigned next_generation = 1;

static void tree_one_time_init(void)
{
   static bool done = false;
   if (likely(done))
      return;

   format_digest = type_format_digest();

   for (int i = 0; i < T_LAST_TREE_KIND; i++) {
      const int nitems = __builtin_popcount(has_map[i]);
      object_size[i]   = sizeof(struct tree) + (nitems * sizeof(item_t));
      object_nitems[i] = nitems;

      // Knuth's multiplicative hash
      format_digest += has_map[i] * 2654435761u;

      int n = 0;
      for (int j = 0; j < 32; j++) {
         if (has_map[i] & (1 << j))
            item_lookup[i][j] = n++;
         else
            item_lookup[i][j] = -1;
      }
   }

   bool changed = false;
   do {
      changed = false;
      for (int i = 0; i < T_LAST_TREE_KIND; i++) {
         size_t max_size = object_size[i];
         for (size_t j = 0; j < ARRAY_LEN(change_allowed); j++) {
            if (change_allowed[j][0] == i)
               max_size = MAX(max_size, object_size[change_allowed[j][1]]);
         }

         if (max_size != object_size[i]) {
            object_size[i] = max_size;
            changed = true;
         }
      }
   } while (changed);

   if (getenv("NVC_TREE_SIZES") != NULL) {
      for (int i = 0; i < T_LAST_TREE_KIND; i++)
         printf("%-15s %d\n", tree_kind_str(i), (int)object_size[i]);
   }

   done = true;
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
      fatal_trace("tree kind %s does not have item %s",
                  kind_text_map[t->kind], item_text_map[item]);
   }

   const int tzc = __builtin_ctz(mask);
   const int n   = item_lookup[t->kind][tzc];

   return &(t->items[n]);
}

static void item_without_type(imask_t mask)
{
   int item;
   for (item = 0; (mask & (1 << item)) == 0; item++)
      ;

   assert(item < ARRAY_LEN(item_text_map));
   fatal_trace("tree item %s does not have a type", item_text_map[item]);
}

static bool tree_kind_in(tree_t t, const tree_kind_t *list, size_t len)
{
   for (size_t i = 0; i < len; i++) {
      if (t->kind == list[i])
         return true;
   }

   return false;
}

static void tree_assert_kind(tree_t t, const tree_kind_t *list, size_t len,
                             const char *what)
{
   if (!tree_kind_in(t, list, len))
      fatal_trace("tree kind %s is not %s", tree_kind_str(t->kind), what);
}

static void tree_assert_stmt(tree_t t)
{
   tree_assert_kind(t, stmt_kinds, ARRAY_LEN(stmt_kinds), "a statement");
}

static void tree_assert_expr(tree_t t)
{
   tree_assert_kind(t, expr_kinds, ARRAY_LEN(expr_kinds), "an expression");
}

static void tree_assert_decl(tree_t t)
{
   tree_assert_kind(t, decl_kinds, ARRAY_LEN(decl_kinds), "a declaration");
}

tree_t tree_new(tree_kind_t kind)
{
   assert(kind < T_LAST_TREE_KIND);

   tree_one_time_init();

   tree_t t = xmalloc(object_size[kind]);
   memset(t, '\0', object_size[kind]);
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

      bool top_level = tree_kind_in(all_trees[i], top_level_kinds,
                                    ARRAY_LEN(top_level_kinds));

      if (top_level) {
         object_visit_ctx_t ctx = {
            .count      = 0,
            .fn         = NULL,
            .context    = NULL,
            .kind       = T_LAST_TREE_KIND,
            .generation = next_generation++,
            .deep       = true
         };

         tree_visit_aux(all_trees[i], &ctx);
      }
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
               else if (ITEM_NETID_ARRAY & mask)
                  free(t->items[n].netid_array.items);
               else if (ITEM_RANGE & mask)
                  free(t->items[n].range);
               n++;
            }
         }

         if (t->attrs.table != NULL)
            free(t->attrs.table);

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

bool tree_has_ident2(tree_t t)
{
   return lookup_item(t, I_IDENT2)->ident != NULL;
}

tree_kind_t tree_kind(tree_t t)
{
   assert(t != NULL);
   return t->kind;
}

void tree_change_kind(tree_t t, tree_kind_t kind)
{
   assert(t != NULL);

   bool allow = false;
   for (size_t i = 0; (i < ARRAY_LEN(change_allowed)) && !allow; i++) {
      allow = (change_allowed[i][0] == t->kind)
         && (change_allowed[i][1] == kind);
   }

   if (!allow)
      fatal_trace("cannot change tree kind %s to %s",
                  tree_kind_str(t->kind), tree_kind_str(kind));

   const uint32_t old_has = has_map[t->kind];
   const uint32_t new_has = has_map[kind];

   const int old_nitems = __builtin_popcount(old_has);
   const int new_nitems = __builtin_popcount(new_has);

   const int max_items = MAX(old_nitems, new_nitems);

   item_t tmp[max_items];
   memcpy(tmp, t->items, sizeof(item_t) * max_items);

   int op = 0, np = 0;
   for (uint32_t mask = 1; np < new_nitems; mask <<= 1) {
      if ((old_has & mask) && (new_has & mask))
         t->items[np++] = tmp[op++];
      else if (old_has & mask)
         ++op;
      else if (new_has & mask)
         memset(&(t->items[np++]), '\0', sizeof(item_t));
   }

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
   tree_assert_decl(d);
   tree_array_add(&(lookup_item(t, I_PORTS)->tree_array), d);
}

unsigned tree_subkind(tree_t t)
{
   item_t *item = lookup_item(t, I_SUBKIND);
   return item->ival;
}

void tree_set_subkind(tree_t t, unsigned sub)
{
   lookup_item(t, I_SUBKIND)->ival = sub;
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
   tree_assert_decl(d);
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
   return lookup_item(t, I_PARAMS)->tree_array.count;
}

tree_t tree_param(tree_t t, unsigned n)
{
   return tree_array_nth(&(lookup_item(t, I_PARAMS)->tree_array), n);
}

void tree_add_param(tree_t t, tree_t e)
{
   assert(tree_kind(e) == T_PARAM);
   tree_assert_expr(tree_value(e));

   tree_array_t *array = &(lookup_item(t, I_PARAMS)->tree_array);

   if (tree_subkind(e) == P_POS)
      tree_set_pos(e, array->count);

   tree_array_add(array, e);
}

unsigned tree_genmaps(tree_t t)
{
   return lookup_item(t, I_GENMAPS)->tree_array.count;
}

tree_t tree_genmap(tree_t t, unsigned n)
{
   return tree_array_nth(&(lookup_item(t, I_GENMAPS)->tree_array), n);
}

void tree_add_genmap(tree_t t, tree_t e)
{
   tree_assert_expr(tree_value(e));

   tree_array_t *array = &(lookup_item(t, I_GENMAPS)->tree_array);

   if (tree_subkind(e) == P_POS)
      tree_set_pos(e, array->count);

   tree_array_add(&(lookup_item(t, I_GENMAPS)->tree_array), e);
}

int64_t tree_ival(tree_t t)
{
   assert((t->kind == T_LITERAL) && (tree_subkind(t) == L_INT));
   return lookup_item(t, I_IVAL)->ival;
}

void tree_set_ival(tree_t t, int64_t i)
{
   assert((t->kind == T_LITERAL) && (tree_subkind(t) == L_INT));
   lookup_item(t, I_IVAL)->ival = i;
}

double tree_dval(tree_t t)
{
   assert((t->kind == T_LITERAL) && (tree_subkind(t) == L_REAL));
   return lookup_item(t, I_DVAL)->dval;
}

void tree_set_dval(tree_t t, double d)
{
   assert((t->kind == T_LITERAL) && (tree_subkind(t) == L_REAL));
   lookup_item(t, I_DVAL)->dval = d;
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
   if ((v != NULL) && (t->kind != T_ASSOC))
      tree_assert_expr(v);
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
   tree_assert_decl(d);
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
   tree_assert_stmt(s);
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
   assert(w->kind == T_WAVEFORM);
   tree_array_add(&(lookup_item(t, I_WAVES)->tree_array), w);
}

unsigned tree_else_stmts(tree_t t)
{
   return lookup_item(t, I_ELSES)->tree_array.count;
}

tree_t tree_else_stmt(tree_t t, unsigned n)
{
   return tree_array_nth(&(lookup_item(t, I_ELSES)->tree_array), n);
}

void tree_add_else_stmt(tree_t t, tree_t s)
{
   tree_assert_stmt(s);
   tree_array_add(&(lookup_item(t, I_ELSES)->tree_array), s);
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
   assert(c->kind == T_COND);
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
   tree_assert_expr(d);
   lookup_item(t, I_DELAY)->tree = d;
}

unsigned tree_triggers(tree_t t)
{
   return lookup_item(t, I_TRIGGERS)->tree_array.count;
}

tree_t tree_trigger(tree_t t, unsigned n)
{
   return tree_array_nth(&(lookup_item(t, I_TRIGGERS)->tree_array), n);
}

void tree_add_trigger(tree_t t, tree_t s)
{
   tree_assert_expr(s);
   tree_array_add(&(lookup_item(t, I_TRIGGERS)->tree_array), s);
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
   assert(tree_kind_in(decl, decl_kinds, ARRAY_LEN(decl_kinds))
          || (decl->kind == T_ENUM_LIT)
          || tree_kind_in(decl, top_level_kinds, ARRAY_LEN(top_level_kinds)));
   lookup_item(t, I_REF)->tree = decl;
}

unsigned tree_contexts(tree_t t)
{
   return lookup_item(t, I_CONTEXT)->tree_array.count;
}

tree_t tree_context(tree_t t, unsigned n)
{
   return tree_array_nth(&(lookup_item(t, I_CONTEXT)->tree_array), n);
}

void tree_add_context(tree_t t, tree_t ctx)
{
   assert(ctx->kind == T_CONTEXT);
   tree_array_add(&(lookup_item(t, I_CONTEXT)->tree_array), ctx);
}

unsigned tree_assocs(tree_t t)
{
   return lookup_item(t, I_ASSOCS)->tree_array.count;
}

tree_t tree_assoc(tree_t t, unsigned n)
{
   return tree_array_nth(&(lookup_item(t, I_ASSOCS)->tree_array), n);
}

void tree_add_assoc(tree_t t, tree_t a)
{
   assert(a->kind == T_ASSOC);

   tree_array_t *array = &(lookup_item(t, I_ASSOCS)->tree_array);

   if (tree_subkind(a) == A_POS)
      tree_set_pos(a, array->count);

   tree_array_add(array, a);
}

unsigned tree_nets(tree_t t)
{
   return lookup_item(t, I_NETS)->netid_array.count;
}

netid_t tree_net(tree_t t, unsigned n)
{
   netid_t nid = netid_array_nth(&(lookup_item(t, I_NETS)->netid_array), n);
   assert(nid != NETID_INVALID);
   return nid;
}

void tree_add_net(tree_t t, netid_t n)
{
   netid_array_add(&(lookup_item(t, I_NETS)->netid_array), n);
}

void tree_change_net(tree_t t, unsigned n, netid_t i)
{
   item_t *item = lookup_item(t, I_NETS);

   if (n >= item->netid_array.count)
      netid_array_resize(&(item->netid_array), n + 1, NETID_INVALID);

   item->netid_array.items[n] = i;
}

tree_t tree_severity(tree_t t)
{
   item_t *item = lookup_item(t, I_SEVERITY);
   assert(item->tree != NULL);
   return item->tree;
}

void tree_set_severity(tree_t t, tree_t s)
{
   tree_assert_expr(s);
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
   tree_assert_expr(m);
   lookup_item(t, I_MESSAGE)->tree = m;
}

range_t tree_range(tree_t t)
{
   return *(lookup_item(t, I_RANGE)->range);
}

void tree_set_range(tree_t t, range_t r)
{
   item_t *item = lookup_item(t, I_RANGE);
   if (item->range == NULL)
      item->range = xmalloc(sizeof(range_t));
   *(item->range) = r;
}

unsigned tree_pos(tree_t t)
{
   return lookup_item(t, I_POS)->ival;
}

void tree_set_pos(tree_t t, unsigned pos)
{
   lookup_item(t, I_POS)->ival = pos;
}

class_t tree_class(tree_t t)
{
   return lookup_item(t, I_CLASS)->ival;
}

void tree_set_class(tree_t t, class_t c)
{
   lookup_item(t, I_CLASS)->ival = c;
}

tree_t tree_reject(tree_t t)
{
   item_t *item = lookup_item(t, I_REJECT);
   assert(item->tree != NULL);
   return item->tree;
}

void tree_set_reject(tree_t t, tree_t r)
{
   tree_assert_expr(r);
   lookup_item(t, I_REJECT)->tree = r;
}

tree_t tree_name(tree_t t)
{
   item_t *item = lookup_item(t, I_NAME);
   assert(item->tree != NULL);
   return item->tree;
}

void tree_set_name(tree_t t, tree_t n)
{
   tree_assert_expr(n);
   lookup_item(t, I_NAME)->tree = n;
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

void tree_visit_aux(tree_t t, object_visit_ctx_t *ctx)
{
   // If `deep' then will follow links above the tree originally passed
   // to tree_visit - e.g. following references back to their declarations
   // Outside the garbage collector this is usually not what is required

   // Helper from type.c

   if (t == NULL || t->generation == ctx->generation)
      return;

   t->generation = ctx->generation;

   const imask_t deep_mask = I_TYPE | I_REF;

   const imask_t has = has_map[t->kind];
   const int nitems = __builtin_popcount(has);
   imask_t mask = 1;
   for (int i = 0; i < nitems; mask <<= 1) {
      if (has & mask & ~(ctx->deep ? 0 : deep_mask)) {
         if (ITEM_IDENT & mask)
            ;
         else if (ITEM_TREE & mask)
            tree_visit_aux(t->items[i].tree, ctx);
         else if (ITEM_TREE_ARRAY & mask) {
            for (unsigned j = 0; j < t->items[i].tree_array.count; j++)
               tree_visit_aux(t->items[i].tree_array.items[j], ctx);
         }
         else if (ITEM_TYPE & mask)
            type_visit_trees(t->items[i].type, ctx);
         else if (ITEM_INT64 & mask)
            ;
         else if (ITEM_DOUBLE & mask)
            ;
         else if (ITEM_RANGE & mask) {
            if (t->items[i].range != NULL) {
               tree_visit_aux(t->items[i].range->left, ctx);
               tree_visit_aux(t->items[i].range->right, ctx);
            }
         }
         else if (ITEM_NETID_ARRAY & mask)
            ;
         else
            item_without_type(mask);
      }

      if (has & mask)
         i++;
   }

   if (ctx->deep) {
      for (unsigned i = 0; i < t->attrs.num; i++) {
         switch (t->attrs.table[i].kind) {
         case A_TREE:
            tree_visit_aux(t->attrs.table[i].tval, ctx);
            break;

         default:
            break;
         }
      }
   }

   if ((t->kind == ctx->kind) || (ctx->kind == T_LAST_TREE_KIND)) {
      if (ctx->fn)
         (*ctx->fn)(t, ctx->context);
      ctx->count++;
   }
}

unsigned tree_visit(tree_t t, tree_visit_fn_t fn, void *context)
{
   assert(t != NULL);

   object_visit_ctx_t ctx = {
      .count      = 0,
      .fn         = fn,
      .context    = context,
      .kind       = T_LAST_TREE_KIND,
      .generation = next_generation++,
      .deep       = false
   };

   tree_visit_aux(t, &ctx);

   return ctx.count;
}

unsigned tree_visit_only(tree_t t, tree_visit_fn_t fn,
                         void *context, tree_kind_t kind)
{
   assert(t != NULL);

   object_visit_ctx_t ctx = {
      .count      = 0,
      .fn         = fn,
      .context    = context,
      .kind       = kind,
      .generation = next_generation++,
      .deep       = false
   };

   tree_visit_aux(t, &ctx);

   return ctx.count;
}

static void write_loc(loc_t *l, tree_wr_ctx_t ctx)
{
   if (l->file == NULL) {
      write_u16(0xfffe, ctx->file);  // Invalid location marker
      return;
   }

   uint16_t findex;
   for (findex = 0;
        (findex < MAX_FILES)
           && (ctx->file_names[findex] != NULL)
           && (strcmp(ctx->file_names[findex], l->file) != 0);
        findex++)
      ;
   assert(findex != MAX_FILES);

   if (ctx->file_names[findex] == NULL) {
      const size_t len = strlen(l->file) + 1;

      ctx->file_names[findex] = l->file;

      write_u16(findex | 0x8000, ctx->file);
      write_u16(len, ctx->file);
      write_raw(l->file, len, ctx->file);
   }
   else
      write_u16(findex, ctx->file);

   write_u16(l->first_line, ctx->file);
   write_u16(l->first_column, ctx->file);
   write_u16(l->last_line, ctx->file);
   write_u16(l->last_column, ctx->file);
}

static loc_t read_loc(tree_rd_ctx_t ctx)
{
   const char *fname;
   uint16_t fmarker = read_u16(ctx->file);
   if (fmarker == 0xfffe)
      return LOC_INVALID;
   else if (fmarker & 0x8000) {
      uint16_t index = fmarker & 0x7fff;
      assert(index < MAX_FILES);
      uint16_t len = read_u16(ctx->file);
      char *buf = xmalloc(len);
      read_raw(buf, len, ctx->file);

      ctx->file_names[index] = buf;
      fname = buf;
   }
   else {
      assert(fmarker < MAX_FILES);
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

static void write_a(tree_array_t *a, tree_wr_ctx_t ctx)
{
   write_u32(a->count, ctx->file);
   for (unsigned i = 0; i < a->count; i++)
      tree_write(a->items[i], ctx);
}

static void read_a(tree_array_t *a, tree_rd_ctx_t ctx)
{
   tree_array_resize(a, read_u32(ctx->file), NULL);
   for (unsigned i = 0; i < a->count; i++)
      a->items[i] = tree_read(ctx);
}

tree_wr_ctx_t tree_write_begin(fbuf_t *f)
{
   write_u32(format_digest, f);

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

fbuf_t *tree_write_file(tree_wr_ctx_t ctx)
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
         else if (ITEM_TREE_ARRAY & mask)
            write_a(&(t->items[n].tree_array), ctx);
         else if (ITEM_TYPE & mask)
            type_write(t->items[n].type, ctx->type_ctx);
         else if (ITEM_INT64 & mask)
            write_u64(t->items[n].ival, ctx->file);
         else if (ITEM_RANGE & mask) {
            if (t->items[n].range != NULL) {
               write_u8(t->items[n].range->kind, ctx->file);
               tree_write(t->items[n].range->left, ctx);
               tree_write(t->items[n].range->right, ctx);
            }
            else
               write_u8(0xff, ctx->file);
         }
         else if (ITEM_NETID_ARRAY & mask) {
            const netid_array_t *a = &(t->items[n].netid_array);
            write_u32(a->count, ctx->file);
            for (unsigned i = 0; i < a->count; i++)
               write_u32(a->items[i], ctx->file);
         }
         else if (ITEM_DOUBLE & mask) {
            union { double d; uint64_t i; } u;
            u.d = t->items[n].dval;
            write_u64(u.i, ctx->file);
         }
         else
            item_without_type(mask);
         n++;
      }
   }

   write_u16(t->attrs.num, ctx->file);
   for (unsigned i = 0; i < t->attrs.num; i++) {
      write_u16(t->attrs.table[i].kind, ctx->file);
      ident_write(t->attrs.table[i].name, ctx->ident_ctx);

      switch (t->attrs.table[i].kind) {
      case A_STRING:
         ident_write(t->attrs.table[i].sval, ctx->ident_ctx);
         break;

      case A_INT:
         write_u32(t->attrs.table[i].ival, ctx->file);
         break;

      case A_TREE:
         tree_write(t->attrs.table[i].tval, ctx);
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
         else if (ITEM_TREE_ARRAY & mask)
            read_a(&(t->items[n].tree_array), ctx);
         else if (ITEM_TYPE & mask)
            t->items[n].type = type_read(ctx->type_ctx);
         else if (ITEM_INT64 & mask)
            t->items[n].ival = read_u64(ctx->file);
         else if (ITEM_RANGE & mask) {
            const uint8_t rmarker = read_u8(ctx->file);
            if (rmarker != 0xff) {
               t->items[n].range = xmalloc(sizeof(range_t));
               t->items[n].range->kind  = rmarker;
               t->items[n].range->left  = tree_read(ctx);
               t->items[n].range->right = tree_read(ctx);
            }
         }
         else if (ITEM_NETID_ARRAY & mask) {
            netid_array_t *a = &(t->items[n].netid_array);
            netid_array_resize(a, read_u32(ctx->file), NETID_INVALID);
            for (unsigned i = 0; i < a->count; i++)
               a->items[i] = read_u32(ctx->file);
         }
         else if (ITEM_DOUBLE & mask) {
            union { uint64_t i; double d; } u;
            u.i = read_u64(ctx->file);
            t->items[n].dval = u.d;
         }
         else
            item_without_type(mask);
         n++;
      }
   }

   t->attrs.num = read_u16(ctx->file);
   if (t->attrs.num > 0) {
      t->attrs.alloc = next_power_of_2(t->attrs.num);
      t->attrs.table = xmalloc(sizeof(attr_t) * t->attrs.alloc);
   }

   for (unsigned i = 0; i < t->attrs.num; i++) {
      t->attrs.table[i].kind = read_u16(ctx->file);
      t->attrs.table[i].name = ident_read(ctx->ident_ctx);

      switch (t->attrs.table[i].kind) {
      case A_STRING:
         t->attrs.table[i].sval = ident_read(ctx->ident_ctx);
         break;

      case A_INT:
         t->attrs.table[i].ival = read_u32(ctx->file);
         break;

      case A_TREE:
         t->attrs.table[i].tval = tree_read(ctx);
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

tree_rd_ctx_t tree_read_begin(fbuf_t *f, const char *fname)
{
   tree_one_time_init();

   uint32_t ver = read_u32(f);
   if (ver != format_digest)
      fatal("%s: serialised format digest is %x expected %x. This design "
            "unit uses a library format from an earlier version of "
            PACKAGE_NAME " and should be reanalysed.",
            fname, ver, format_digest);

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
   fbuf_close(ctx->file);
   free(ctx->store);
   free(ctx->db_fname);
   free(ctx);
}

fbuf_t *tree_read_file(tree_rd_ctx_t ctx)
{
   return ctx->file;
}

tree_t tree_read_recall(tree_rd_ctx_t ctx, uint32_t index)
{
   assert(index < ctx->n_trees);
   return ctx->store[index];
}

static attr_t *tree_find_attr(tree_t t, ident_t name, attr_kind_t kind)
{
   assert(t != NULL);
   assert(name != NULL);

   for (unsigned i = 0; i < t->attrs.num; i++) {
      if (t->attrs.table[i].kind == kind && t->attrs.table[i].name == name)
         return &(t->attrs.table[i]);
   }

   return NULL;
}

static attr_t *tree_add_attr(tree_t t, ident_t name, attr_kind_t kind)
{
   assert(t != NULL);
   assert(name != NULL);

   attr_t *a = tree_find_attr(t, name, kind);
   if (a != NULL)
      return a;

   if (t->attrs.table == NULL) {
      t->attrs.alloc = 8;
      t->attrs.table = xmalloc(sizeof(attr_t) * t->attrs.alloc);
   }
   else if (t->attrs.alloc == t->attrs.num) {
      t->attrs.alloc *= 2;
      t->attrs.table = xrealloc(t->attrs.table,
                                sizeof(attr_t) * t->attrs.alloc);
   }

   unsigned i = t->attrs.num++;
   t->attrs.table[i].kind = kind;
   t->attrs.table[i].name = name;

   return &(t->attrs.table[i]);
}

void tree_add_attr_str(tree_t t, ident_t name, ident_t str)
{
   tree_add_attr(t, name, A_STRING)->sval = str;
}

ident_t tree_attr_str(tree_t t, ident_t name)
{
   attr_t *a = tree_find_attr(t, name, A_STRING);
   return a ? a->sval : NULL;
}

void tree_add_attr_int(tree_t t, ident_t name, int n)
{
   tree_add_attr(t, name, A_INT)->ival = n;
}

int tree_attr_int(tree_t t, ident_t name, int def)
{
   attr_t *a = tree_find_attr(t, name, A_INT);
   return a ? a->ival : def;
}

void tree_add_attr_ptr(tree_t t, ident_t name, void *ptr)
{
   tree_add_attr(t, name, A_PTR)->pval = ptr;
}

void *tree_attr_ptr(tree_t t, ident_t name)
{
   attr_t *a = tree_find_attr(t, name, A_PTR);
   return a ? a->pval : NULL;
}

tree_t tree_attr_tree(tree_t t, ident_t name)
{
   attr_t *a = tree_find_attr(t, name, A_TREE);
   return a ? a->tval : NULL;
}

void tree_add_attr_tree(tree_t t, ident_t name, tree_t val)
{
   tree_add_attr(t, name, A_TREE)->tval = val;
}

tree_t tree_rewrite_aux(tree_t t, object_rewrite_ctx_t *ctx)
{
   if (t == NULL)
      return NULL;

   if (t->generation == ctx->generation) {
      // Already rewritten this tree so return the cached version
      return ctx->cache[t->index];
   }

   const imask_t skip_mask = I_REF;

   const imask_t has = has_map[t->kind];
   const int nitems = __builtin_popcount(has);
   int type_item = -1;
   imask_t mask = 1;
   for (int n = 0; n < nitems; mask <<= 1) {
      if (has & mask & ~skip_mask) {
         if (ITEM_IDENT & mask)
            ;
         else if (ITEM_TREE & mask)
            t->items[n].tree = tree_rewrite_aux(t->items[n].tree, ctx);
         else if (ITEM_TREE_ARRAY & mask) {
            tree_array_t *a = &(t->items[n].tree_array);

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
         else if (ITEM_TYPE & mask)
            type_item = n;
         else if (ITEM_INT64 & mask)
            ;
         else if (ITEM_DOUBLE & mask)
            ;
         else if (ITEM_RANGE & mask) {
            range_t *r = t->items[n].range;
            if (r != NULL) {
               r->left  = tree_rewrite_aux(r->left, ctx);
               r->right = tree_rewrite_aux(r->right, ctx);
            }
         }
         else if (ITEM_NETID_ARRAY & mask)
            ;
         else
            item_without_type(mask);
      }

      if (has & mask)
         n++;
   }

   t->generation = ctx->generation;
   t->index      = ctx->index++;

   // Rewrite this tree before we rewrite the type as there may
   // be a circular reference
   ctx->cache[t->index] = (*ctx->fn)(t, ctx->context);

   if (type_item != -1)
      type_rewrite_trees(t->items[type_item].type, ctx);

   return ctx->cache[t->index];
}

tree_t tree_rewrite(tree_t t, tree_rewrite_fn_t fn, void *context)
{
   size_t cache_sz = sizeof(tree_t) * n_trees_alloc;
   tree_t *cache = xmalloc(cache_sz);
   memset(cache, '\0', cache_sz);

   object_rewrite_ctx_t ctx = {
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

bool tree_copy_mark(tree_t t, object_copy_ctx_t *ctx)
{
   if (t == NULL)
      return false;

   if (t->generation == ctx->generation)
      return (t->index != UINT32_MAX);

   t->generation = ctx->generation;
   t->index      = UINT32_MAX;

   bool marked = false;
   const imask_t has = has_map[t->kind];
   const int nitems = __builtin_popcount(has);
   imask_t mask = 1;
   for (int n = 0; n < nitems; mask <<= 1) {
      if (has & mask) {
         if (ITEM_IDENT & mask)
            ;
         else if (ITEM_TREE & mask)
            marked = tree_copy_mark(t->items[n].tree, ctx) || marked;
         else if (ITEM_DOUBLE & mask)
            ;
         else if (ITEM_TREE_ARRAY & mask) {
            tree_array_t *a = &(t->items[n].tree_array);
            for (unsigned i = 0; i < a->count; i++)
               marked = tree_copy_mark(a->items[i], ctx) || marked;
         }
         else if (ITEM_TYPE & mask)
            marked = type_copy_mark(t->items[n].type, ctx) || marked;
         else if (ITEM_INT64 & mask)
            ;
         else if (ITEM_RANGE & mask) {
            if (t->items[n].range != NULL) {
               marked = tree_copy_mark(t->items[n].range->left, ctx) || marked;
               marked = tree_copy_mark(t->items[n].range->right, ctx) || marked;
            }
         }
         else if (ITEM_NETID_ARRAY & mask)
            ;
         else
            item_without_type(mask);
         n++;
      }
   }

   if (!marked)
      marked = (*ctx->callback)(t, ctx->context);

   if (marked)
      t->index = (ctx->index)++;

   return marked;
}

tree_t tree_copy_sweep(tree_t t, object_copy_ctx_t *ctx)
{
   if (t == NULL)
      return NULL;

   assert(t->generation == ctx->generation);

   if (t->index == UINT32_MAX)
      return t;

   assert(t->index < ctx->index);

   if (ctx->copied[t->index] != NULL) {
      // Already copied this tree
      return (tree_t)ctx->copied[t->index];
   }

   tree_t copy = tree_new(t->kind);
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
            copy->items[n].tree = tree_copy_sweep(t->items[n].tree, ctx);
         else if (ITEM_DOUBLE & mask)
            copy->items[n].dval = t->items[n].dval;
         else if (ITEM_TREE_ARRAY & mask) {
            const tree_array_t *from = &(t->items[n].tree_array);
            tree_array_t *to = &(copy->items[n].tree_array);

            tree_array_resize(to, from->count, NULL);

            for (size_t i = 0; i < from->count; i++)
               to->items[i] = tree_copy_sweep(from->items[i], ctx);
         }
         else if (ITEM_TYPE & mask)
            copy->items[n].type = type_copy_sweep(t->items[n].type, ctx);
         else if (ITEM_INT64 & mask)
            copy->items[n].ival = t->items[n].ival;
         else if (ITEM_RANGE & mask) {
            const range_t *from = t->items[n].range;
            if (from != NULL) {
               range_t *to = copy->items[n].range = xmalloc(sizeof(range_t));
               to->kind  = from->kind;
               to->left  = tree_copy_sweep(from->left, ctx);
               to->right = tree_copy_sweep(from->right, ctx);
            }
         }
         else if (ITEM_NETID_ARRAY & mask) {
            const netid_array_t *from = &(t->items[n].netid_array);
            netid_array_t *to = &(copy->items[n].netid_array);

            netid_array_resize(to, from->count, NETID_INVALID);

            for (unsigned i = 0; i < from->count; i++)
               to->items[i] = from->items[i];
         }
         else
            item_without_type(mask);
         n++;
      }
   }

   if ((copy->attrs.num = t->attrs.num) > 0) {
      copy->attrs.alloc = t->attrs.alloc;
      copy->attrs.table = xmalloc(sizeof(attr_t) * copy->attrs.alloc);
      for (unsigned i = 0; i < t->attrs.num; i++)
         copy->attrs.table[i] = t->attrs.table[i];
   }

   return copy;
}

tree_t tree_copy(tree_t t, tree_copy_fn_t fn, void *context)
{
   object_copy_ctx_t ctx = {
      .generation = next_generation++,
      .index      = 0,
      .callback   = fn,
      .context    = context,
      .copied     = NULL
   };

   tree_copy_mark(t, &ctx);

   if (t->index == UINT32_MAX)
      return t;   // Nothing to copy

   ctx.copied = xmalloc(sizeof(void *) * ctx.index);
   memset(ctx.copied, '\0', sizeof(void *) * ctx.index);

   tree_t copy = tree_copy_sweep(t, &ctx);

   free(ctx.copied);
   return copy;
}

const char *tree_kind_str(tree_kind_t t)
{
   return kind_text_map[t];
}

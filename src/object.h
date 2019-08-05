//
//  Copyright (C) 2013-2019  Nick Gasson
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

#ifndef _OBJECT_H
#define _OBJECT_H

#include "util.h"
#include "array.h"
#include "tree.h"
#include "type.h"

#include <stdint.h>

//
// Structures shared between tree and type objects
//

#define ONE_HOT(x) (UINT64_C(1) << (x))

typedef uint64_t imask_t;

#define I_IDENT      ONE_HOT(0)
#define I_VALUE      ONE_HOT(1)
#define I_SEVERITY   ONE_HOT(2)
#define I_MESSAGE    ONE_HOT(3)
#define I_TARGET     ONE_HOT(4)
#define I_IVAL       ONE_HOT(5)
#define I_IDENT2     ONE_HOT(6)
#define I_DECLS      ONE_HOT(7)
#define I_STMTS      ONE_HOT(8)
#define I_PORTS      ONE_HOT(9)
#define I_GENERICS   ONE_HOT(10)
#define I_PARAMS     ONE_HOT(11)
#define I_GENMAPS    ONE_HOT(12)
#define I_WAVES      ONE_HOT(13)
#define I_CONDS      ONE_HOT(14)
#define I_TYPE       ONE_HOT(15)
#define I_SUBKIND    ONE_HOT(16)
#define I_DELAY      ONE_HOT(17)
#define I_REJECT     ONE_HOT(18)
#define I_POS        ONE_HOT(19)
#define I_REF        ONE_HOT(20)
#define I_FILE_MODE  ONE_HOT(21)
#define I_ASSOCS     ONE_HOT(22)
#define I_CONTEXT    ONE_HOT(23)
#define I_TRIGGERS   ONE_HOT(24)
#define I_ELSES      ONE_HOT(25)
#define I_CLASS      ONE_HOT(26)
#define I_RANGES     ONE_HOT(27)
#define I_NAME       ONE_HOT(28)
#define I_NETS       ONE_HOT(29)
#define I_DVAL       ONE_HOT(30)
#define I_SPEC       ONE_HOT(31)
#define I_OPS        ONE_HOT(32)
#define I_INDEXCON   ONE_HOT(33)
#define I_BASE       ONE_HOT(34)
#define I_ELEM       ONE_HOT(35)
#define I_FILE       ONE_HOT(36)
#define I_ACCESS     ONE_HOT(37)
#define I_RESOLUTION ONE_HOT(38)
#define I_RESULT     ONE_HOT(39)
#define I_UNITS      ONE_HOT(40)
#define I_LITERALS   ONE_HOT(41)
#define I_DIMS       ONE_HOT(42)
#define I_FIELDS     ONE_HOT(43)
#define I_TEXT_BUF   ONE_HOT(44)
#define I_ATTRS      ONE_HOT(45)
#define I_PTYPES     ONE_HOT(46)
#define I_CHARS      ONE_HOT(47)
#define I_CONSTR     ONE_HOT(48)
#define I_FLAGS      ONE_HOT(49)
#define I_TEXT       ONE_HOT(50)

#define ITEM_IDENT       (I_IDENT | I_IDENT2)
#define ITEM_TREE        (I_VALUE | I_SEVERITY | I_MESSAGE | I_TARGET \
                          | I_DELAY | I_REJECT | I_REF | I_FILE_MODE  \
                          | I_NAME | I_SPEC | I_RESOLUTION | I_CONSTR)
#define ITEM_TREE_ARRAY  (I_DECLS | I_STMTS | I_PORTS | I_GENERICS | I_WAVES \
                          | I_CONDS | I_TRIGGERS | I_ELSES | I_PARAMS  \
                          | I_GENMAPS | I_ASSOCS | I_CONTEXT | I_OPS \
                          | I_LITERALS | I_FIELDS | I_UNITS | I_CHARS)
#define ITEM_TYPE        (I_TYPE | I_BASE | I_ELEM | I_ACCESS | I_RESULT \
                          | I_FILE)
#define ITEM_INT64       (I_POS | I_IVAL)
#define ITEM_INT32       (I_SUBKIND | I_CLASS | I_FLAGS)
#define ITEM_NETID_ARRAY (I_NETS)
#define ITEM_DOUBLE      (I_DVAL)
#define ITEM_TYPE_ARRAY  (I_PTYPES | I_INDEXCON)
#define ITEM_RANGE_ARRAY (I_RANGES | I_DIMS)
#define ITEM_TEXT_BUF    (I_TEXT_BUF)
#define ITEM_ATTRS       (I_ATTRS)
#define ITEM_TEXT        (I_TEXT)

#define OBJECT_TAG_TREE  0
#define OBJECT_TAG_TYPE  1

DECLARE_ARRAY(netid);
DECLARE_ARRAY(range);
DECLARE_ARRAY(tree);
DECLARE_ARRAY(type);
DECLARE_ARRAY(ident);

#define lookup_item(class, t, mask) ({                                  \
         assert((t) != NULL);                                           \
         assert((mask & (mask - 1)) == 0);                              \
                                                                        \
         const imask_t has = has_map[(t)->object.kind];                 \
                                                                        \
         if (unlikely((has & mask) == 0))                               \
            object_lookup_failed((class)->name, kind_text_map,          \
                                 (t)->object.kind, mask);               \
                                                                        \
         const int tzc = __builtin_ctzll(mask);                         \
         const int off = ((t)->object.kind * 64) + tzc;                 \
         const int n   = (class)->item_lookup[off];                     \
                                                                        \
         &((t)->object.items[n]);                                       \
      })

typedef enum {
   A_STRING, A_INT, A_PTR, A_TREE
} attr_kind_t;

typedef uint16_t generation_t;
typedef uint32_t index_t;

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
   range_array_t  range_array;
   text_buf_t    *text_buf;
   type_array_t   type_array;
   attr_tab_t     attrs;
   ident_array_t  ident_array;
   char          *text;
} item_t;

typedef struct {
   uint8_t      kind;
   uint8_t      tag;
   generation_t generation;
   index_t      index;
   loc_t        loc;
   item_t       items[0];
} object_t;

typedef struct {
   generation_t    generation;
   index_t         index;
   tree_copy_fn_t  callback;
   void           *context;
   object_t      **copied;
} object_copy_ctx_t;

typedef struct {
   object_t        **cache;
   index_t           index;
   generation_t      generation;
   tree_rewrite_fn_t fn;
   void             *context;
   size_t            cache_size;
} object_rewrite_ctx_t;

typedef struct {
   unsigned         count;
   tree_visit_fn_t  preorder;
   tree_visit_fn_t  postorder;
   void            *context;
   tree_kind_t      kind;
   unsigned         generation;
   bool             deep;
} object_visit_ctx_t;

typedef int change_allowed_t[2];

typedef struct {
   const char             *name;
   const change_allowed_t *change_allowed;
   const imask_t          *has_map;
   const char            **kind_text_map;
   const int               tag;
   const int               last_kind;
   const int               gc_roots[6];
   const int               gc_num_roots;
   int                    *object_nitems;
   size_t                 *object_size;
   int                    *item_lookup;
} object_class_t;

typedef struct {
   fbuf_t         *file;
   ident_wr_ctx_t  ident_ctx;
   unsigned        generation;
   unsigned        n_objects;
} object_wr_ctx_t;

typedef struct {
   fbuf_t         *file;
   ident_rd_ctx_t  ident_ctx;
   unsigned        n_objects;
   object_t      **store;
   unsigned        store_sz;
   char           *db_fname;
} object_rd_ctx_t;

__attribute__((noreturn))
void object_lookup_failed(const char *name, const char **kind_text_map,
                          int kind, imask_t mask);

void item_without_type(imask_t mask);

void object_change_kind(const object_class_t *class,
                        object_t *object, int kind);
object_t *object_new(const object_class_t *class, int kind);
void object_one_time_init(void);
void object_gc(void);
void object_visit(object_t *object, object_visit_ctx_t *ctx);
object_t *object_rewrite(object_t *object, object_rewrite_ctx_t *ctx);
unsigned object_next_generation(void);
object_t *object_copy_sweep(object_t *object, object_copy_ctx_t *ctx);
bool object_copy_mark(object_t *object, object_copy_ctx_t *ctx);
void object_replace(object_t *t, object_t *a);

void object_write(object_t *object, object_wr_ctx_t *ctx);
object_wr_ctx_t *object_write_begin(fbuf_t *f);
void object_write_end(object_wr_ctx_t *ctx);

object_rd_ctx_t *object_read_begin(fbuf_t *f, const char *fname);
void object_read_end(object_rd_ctx_t *ctx);
object_t *object_read(object_rd_ctx_t *ctx, int tag);

#endif   // _OBJECT_H

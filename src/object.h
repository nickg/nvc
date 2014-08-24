//
//  Copyright (C) 2013-2014  Nick Gasson
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
#define I_RANGE      ONE_HOT(27)
#define I_NAME       ONE_HOT(28)
#define I_NETS       ONE_HOT(29)
#define I_DVAL       ONE_HOT(30)
#define I_SPEC       ONE_HOT(31)
#define I_OPS        ONE_HOT(32)
#define I_CONSTR     ONE_HOT(33)
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

#define ITEM_IDENT       (I_IDENT | I_IDENT2)
#define ITEM_TREE        (I_VALUE | I_SEVERITY | I_MESSAGE | I_TARGET \
                          | I_DELAY | I_REJECT | I_REF | I_FILE_MODE  \
                          | I_NAME | I_SPEC | I_RESOLUTION)
#define ITEM_TREE_ARRAY  (I_DECLS | I_STMTS | I_PORTS | I_GENERICS | I_WAVES \
                          | I_CONDS | I_TRIGGERS | I_ELSES | I_PARAMS  \
                          | I_GENMAPS | I_ASSOCS | I_CONTEXT | I_OPS \
                          | I_LITERALS | I_FIELDS | I_UNITS)
#define ITEM_TYPE        (I_TYPE | I_BASE | I_ELEM | I_ACCESS | I_RESULT \
                          | I_FILE)
#define ITEM_INT64       (I_POS | I_SUBKIND | I_CLASS | I_IVAL)
#define ITEM_RANGE       (I_RANGE)
#define ITEM_NETID_ARRAY (I_NETS)
#define ITEM_DOUBLE      (I_DVAL)
#define ITEM_TYPE_ARRAY  (I_PARAMS | I_CONSTR)
#define ITEM_RANGE_ARRAY (I_DIMS)
#define ITEM_TEXT_BUF    (I_TEXT_BUF)

DECLARE_ARRAY(tree);
DECLARE_ARRAY(netid);
DECLARE_ARRAY(type);
DECLARE_ARRAY(range);

#define lookup_item(t, mask) ({                                         \
         assert(t != NULL);                                             \
         assert((mask & (mask - 1)) == 0);                              \
                                                                        \
         const imask_t has = has_map[t->kind];                          \
                                                                        \
         if (unlikely((has & mask) == 0))                               \
            object_lookup_failed(OBJECT_NAME, kind_text_map,            \
                                 t->kind, mask);                        \
                                                                        \
         const int tzc = __builtin_ctzll(mask);                         \
         const int n   = item_lookup[t->kind][tzc];                     \
                                                                        \
         &(t->items[n]);                                                \
      })

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
} item_t;

typedef struct {
   uint32_t        generation;
   uint32_t        index;
   tree_copy_fn_t  callback;
   void           *context;
   void          **copied;
} object_copy_ctx_t;

typedef struct {
   tree_t            *cache;
   uint32_t          index;
   uint32_t          generation;
   tree_rewrite_fn_t fn;
   void              *context;
} object_rewrite_ctx_t;

typedef struct {
   unsigned         count;
   tree_visit_fn_t  fn;
   void            *context;
   tree_kind_t      kind;
   unsigned         generation;
   bool             deep;
} object_visit_ctx_t;

typedef struct type_wr_ctx *type_wr_ctx_t;
typedef struct type_rd_ctx *type_rd_ctx_t;

struct tree_wr_ctx;
struct tree_rd_ctx;

bool tree_copy_mark(tree_t t, object_copy_ctx_t *ctx);
bool type_copy_mark(type_t t, object_copy_ctx_t *ctx);

tree_t tree_copy_sweep(tree_t t, object_copy_ctx_t *ctx);
type_t type_copy_sweep(type_t t, object_copy_ctx_t *ctx);

void type_sweep(unsigned generation);

type_wr_ctx_t type_write_begin(struct tree_wr_ctx *tree_ctx,
                               ident_wr_ctx_t ident_ctx);
void type_write(type_t t, type_wr_ctx_t ctx);
void type_write_end(type_wr_ctx_t ctx);

type_rd_ctx_t type_read_begin(struct tree_rd_ctx *tree_ctx,
                              ident_rd_ctx_t ident_ctx);
type_t type_read(type_rd_ctx_t ctx);
void type_read_end(type_rd_ctx_t ctx);

tree_t tree_rewrite_aux(tree_t t, object_rewrite_ctx_t *ctx);
void type_rewrite_trees(type_t t, object_rewrite_ctx_t *ctx);

void tree_visit_aux(tree_t t, object_visit_ctx_t *ctx);
void type_visit_trees(type_t t, object_visit_ctx_t *ctx);

__attribute__((noreturn))
void object_lookup_failed(const char *name, const char **kind_text_map,
                          int kind, imask_t mask);

void item_without_type(imask_t mask);

#endif   // _OBJECT_H

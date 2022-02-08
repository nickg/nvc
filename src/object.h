//
//  Copyright (C) 2013-2022  Nick Gasson
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
#include "prim.h"
#include "array.h"
#include "tree.h"
#include "loc.h"

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
// Unused            ONE_HOT(25)
#define I_CLASS      ONE_HOT(26)
#define I_RANGES     ONE_HOT(27)
#define I_NAME       ONE_HOT(28)
// Unused            ONE_HOT(29)
#define I_DVAL       ONE_HOT(30)
#define I_SPEC       ONE_HOT(31)
#define I_SCOPES     ONE_HOT(32)
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
#define I_PARENT     ONE_HOT(44)
#define I_GUARD      ONE_HOT(45)
#define I_PTYPES     ONE_HOT(46)
#define I_CHARS      ONE_HOT(47)
#define I_CONSTR     ONE_HOT(48)
#define I_FLAGS      ONE_HOT(49)
#define I_SIGNALS    ONE_HOT(50)
#define I_LEFT       ONE_HOT(51)
#define I_RIGHT      ONE_HOT(52)
#define I_PROCS      ONE_HOT(53)
#define I_NEXUS      ONE_HOT(54)
#define I_PATH       ONE_HOT(55)
#define I_DEPS       ONE_HOT(56)
// Unused            ONE_HOT(57)
#define I_VCODE      ONE_HOT(58)
#define I_PRIMARY    ONE_HOT(59)
#define I_SOURCES    ONE_HOT(60)
#define I_OUTPUTS    ONE_HOT(61)

#define ITEM_IDENT       (I_IDENT | I_IDENT2 | I_PATH | I_VCODE)
#define ITEM_OBJECT      (I_VALUE | I_SEVERITY | I_MESSAGE | I_TARGET   \
                          | I_DELAY | I_REJECT | I_REF | I_FILE_MODE    \
                          | I_NAME | I_SPEC | I_RESOLUTION              \
                          | I_LEFT | I_RIGHT | I_TYPE | I_BASE | I_ELEM \
                          | I_ACCESS | I_RESULT | I_FILE | I_PARENT     \
                          | I_PRIMARY | I_GUARD)
#define ITEM_OBJ_ARRAY   (I_DECLS | I_STMTS | I_PORTS | I_GENERICS      \
                          | I_WAVES | I_CONDS | I_TRIGGERS | I_CONSTR   \
                          | I_PARAMS | I_GENMAPS | I_ASSOCS | I_CONTEXT \
                          | I_LITERALS | I_FIELDS | I_OUTPUTS           \
                          | I_UNITS | I_CHARS | I_DIMS | I_RANGES       \
                          | I_PTYPES | I_INDEXCON | I_SIGNALS | I_PROCS \
                          | I_NEXUS | I_SCOPES | I_SOURCES)
#define ITEM_IDENT_ARRAY (I_DEPS)
#define ITEM_INT64       (I_POS | I_IVAL)
#define ITEM_INT32       (I_SUBKIND | I_CLASS | I_FLAGS)
#define ITEM_DOUBLE      (I_DVAL)

enum {
   OBJECT_TAG_TREE   = 0,
   OBJECT_TAG_TYPE   = 1,
   OBJECT_TAG_E_NODE = 2,

   OBJECT_TAG_COUNT
};

#define OBJECT_PAGE_SZ       (1 << 16)
#define OBJECT_PAGE_MASK     (OBJECT_PAGE_SZ - 1)
#define OBJECT_ALIGN_BITS    4
#define OBJECT_ALIGN         (1 << OBJECT_ALIGN_BITS)
#define OBJECT_UNMAP_UNUSED  1

STATIC_ASSERT(OBJECT_ALIGN >= sizeof(double));

#define lookup_item(class, t, mask) ({                                  \
         assert((t) != NULL);                                           \
         assert((mask & (mask - 1)) == 0);                              \
                                                                        \
         const imask_t __has = has_map[(t)->object.kind];               \
                                                                        \
         if (unlikely((__has & (mask)) == 0))                           \
            object_lookup_failed((class)->name, kind_text_map,          \
                                 (t)->object.kind, mask);               \
                                                                        \
         const int __tzc = __builtin_ctzll(mask);                       \
         const int __off = ((t)->object.kind * 64) + __tzc;             \
         const int __n   = (class)->item_lookup[__off];                 \
                                                                        \
         &((t)->object.items[__n]);                                     \
      })

typedef uint16_t generation_t;
typedef uint16_t arena_key_t;

typedef A(object_t *) obj_array_t;

typedef union {
   ident_t       ident;
   object_t     *object;
   obj_array_t   obj_array;
   type_t        type;
   unsigned      subkind;
   int64_t       ival;
   double        dval;
   text_buf_t   *text_buf;
   char         *text;
   A(ident_t)    ident_array;
} item_t;

struct _object {
   uint8_t      kind;
   uint8_t      tag;
   arena_key_t  arena;
   uint16_t     __pad;
   loc_t        loc;
   item_t       items[0];
};

STATIC_ASSERT(sizeof(object_t) == 16);

typedef bool (*object_copy_pred_t)(object_t *, void *);
typedef bool (*object_copy_fn_t)(object_t *, void *);

typedef struct {
   generation_t       generation;
   object_copy_pred_t should_copy[OBJECT_TAG_COUNT];
   object_copy_fn_t   callback[OBJECT_TAG_COUNT];
   void              *context;
   object_arena_t    *arena;
   hash_t            *copy_map;
   unsigned           nroots;
   object_t          *roots[0];
} object_copy_ctx_t;

typedef void (*object_rewrite_pre_fn_t)(object_t *, void *);
typedef object_t *(*object_rewrite_post_fn_t)(object_t *, void *);

typedef struct {
   object_t                 **cache;
   generation_t               generation;
   object_rewrite_pre_fn_t    pre_fn[OBJECT_TAG_COUNT];
   object_rewrite_post_fn_t   post_fn[OBJECT_TAG_COUNT];
   void                      *context;
   object_arena_t            *arena;
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
   const int               gc_roots[10];
   const int               gc_num_roots;
   int                    *object_nitems;
   size_t                 *object_size;
   int                    *item_lookup;
} object_class_t;

typedef object_t *(*object_load_fn_t)(ident_t);

__attribute__((noreturn))
void object_lookup_failed(const char *name, const char **kind_text_map,
                          int kind, imask_t mask);

void item_without_type(imask_t mask);

void object_change_kind(const object_class_t *class,
                        object_t *object, int kind);
object_t *object_new(object_arena_t *arena,
                     const object_class_t *class, int kind);
void object_one_time_init(void);
void object_visit(object_t *object, object_visit_ctx_t *ctx);
object_t *object_rewrite(object_t *object, object_rewrite_ctx_t *ctx);
unsigned object_next_generation(void);
void object_copy(object_copy_ctx_t *ctx);
object_arena_t *object_arena(object_t *object);
size_t object_arena_default_size(void);
object_t *arena_root(object_arena_t *arena);

void object_write(object_t *object, fbuf_t *f, ident_wr_ctx_t ident_ctx,
                  loc_wr_ctx_t *loc_ctx);
object_t *object_read(fbuf_t *f, object_load_fn_t loader,
                      ident_rd_ctx_t ident_ctx, loc_rd_ctx_t *loc_ctx);

#define object_write_barrier(lhs, rhs) do {                     \
      uintptr_t __lp = (uintptr_t)(lhs) & ~OBJECT_PAGE_MASK;    \
      uintptr_t __rp = (uintptr_t)(rhs) & ~OBJECT_PAGE_MASK;    \
      if (__lp != __rp && (rhs) != NULL)                        \
         __object_write_barrier((lhs), (rhs));                  \
   } while (0);

void __object_write_barrier(object_t *lhs, object_t *rhs);

object_arena_t *object_arena_new(size_t size, unsigned std);
void object_arena_freeze(object_arena_t *arena);

typedef void (*object_arena_deps_fn_t)(ident_t, void *);
void object_arena_walk_deps(object_arena_t *arena, object_arena_deps_fn_t fn,
                            void *context);

void object_locus(object_t *object, ident_t *module, ptrdiff_t *offset);
object_t *object_from_locus(ident_t module, ptrdiff_t offset,
                            object_load_fn_t loader, unsigned tag);

#endif   // _OBJECT_H

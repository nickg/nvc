//
//  Copyright (C) 2013-2021  Nick Gasson
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

#ifndef _PRIM_H
#define _PRIM_H

#include <stdint.h>
#include <stddef.h>


typedef struct _lib *lib_t;
typedef struct _object object_t;
typedef struct _object_arena object_arena_t;
typedef struct trie *ident_t;
typedef struct _tree *tree_t;
typedef struct _type *type_t;
typedef struct _e_node *e_node_t;
typedef struct loc loc_t;
typedef struct _fbuf fbuf_t;
typedef struct _hash hash_t;
typedef struct _shash shash_t;
typedef struct _exec exec_t;
typedef struct _eval_frame eval_frame_t;
typedef struct text_buf text_buf_t;

typedef struct vcode_unit *vcode_unit_t;

typedef struct _cover_tagging cover_tagging_t;

#endif  // _PRIM_H

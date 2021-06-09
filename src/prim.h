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

typedef struct trie *ident_t;
typedef struct _tree *tree_t;
typedef struct _type *type_t;
typedef struct _e_node *e_node_t;
typedef struct loc loc_t;
typedef struct _fbuf fbuf_t;

typedef struct tree_wr_ctx *tree_wr_ctx_t;
typedef struct tree_rd_ctx *tree_rd_ctx_t;

typedef struct vcode_unit *vcode_unit_t;

typedef struct _cover_tagging cover_tagging_t;

#endif  // _PRIM_H

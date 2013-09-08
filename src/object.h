//
//  Copyright (C) 2013  Nick Gasson
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

//
// Structures shared between tree and type objects
//

typedef struct {
   uint32_t        generation;
   uint32_t        index;
   tree_copy_fn_t  callback;
   void           *context;
   void          **copied;
} object_copy_ctx_t;

bool tree_copy_mark(tree_t t, object_copy_ctx_t *ctx);
bool type_copy_mark(type_t t, object_copy_ctx_t *ctx);

tree_t tree_copy_sweep(tree_t t, object_copy_ctx_t *ctx);
type_t type_copy_sweep(type_t t, object_copy_ctx_t *ctx);

#endif   // _OBJECT_H

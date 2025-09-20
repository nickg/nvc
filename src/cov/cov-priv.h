//
//  Copyright (C) 2013-2025  Nick Gasson
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

#ifndef _COV_PRIV_H
#define _COV_PRIV_H

#include "prim.h"
#include "cov/cov-api.h"
#include "cov/cov-structs.h"

block_kind_t cover_block_kind(cover_scope_t *s);
bool cover_is_hier(cover_scope_t *s);

#endif   // _COV_PRIV_H

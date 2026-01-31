//
//  Copyright (C) 2013-2026  Nick Gasson
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

bool cover_is_hier(cover_scope_t *s);
bool cover_is_leaf(cover_scope_t *s);
bool cover_bin_unreachable(cover_data_t *data, const cover_item_t *item);

bool cover_should_emit_scope(cover_data_t *db, cover_scope_t *cs);
bool cover_should_emit_fsm_type(cover_data_t *db, ident_t name);

const rpt_file_t *rpt_get_file(cover_rpt_t *rpt, cover_scope_t *s);
const rpt_hier_t *rpt_get_hier(cover_rpt_t *rpt, cover_scope_t *s);
unsigned rpt_get_skipped(cover_rpt_t *rpt);

typedef void (*rpt_file_fn_t)(const rpt_file_t *, void *);
int rpt_iter_files(cover_rpt_t *rpt, rpt_file_fn_t fn, void *ctx);

#endif   // _COV_PRIV_H

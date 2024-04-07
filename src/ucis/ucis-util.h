//
//  Copyright (C) 2024  Nick Gasson
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

#ifndef _UCIS_UTIL_H
#define _UCIS_UTIL_H

#include "prim.h"
#include "ucis/ucis-api.h"
#include "ucis/ucis-structs.h"

#include <stdio.h>
#include <time.h>

int ucis_error(const char *fmt, ...);
int ucis_populate(ucisT db, const char *path);
ucisCoverItemT *ucis_get_cover_item(ucisScopeT scope, int index);
bool ucis_is_design_unit(ucisScopeT scope);
ucisObjT ucis_new_object(ucisObjMaskT kind, size_t size);
ucisStringT ucis_new_string(ucisT db, const char *str);
const char *ucis_get_string(ucisT db, ucisStringT str);
ucisScopeT ucis_cast_scope(ucisObjT obj);

#endif   // _UCIS_UTIL_H

//
//  Copyright (C) 2023  Nick Gasson
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

#ifndef _DRIVER_H
#define _DRIVER_H

#include "prim.h"

typedef struct _driver_info driver_info_t;

typedef struct _driver_info {
   driver_info_t *chain_proc;
   driver_info_t *chain_decl;
   tree_t         decl;
   tree_t         where;
   tree_t         prefix;
   bool           tentative;
} driver_info_t;

driver_set_t *find_drivers(tree_t block);
driver_info_t *get_drivers(driver_set_t *ds, tree_t what);
bool has_unique_driver(driver_set_t *ds, tree_t what);
void free_drivers(driver_set_t *ds);
void dump_drivers(driver_set_t *ds);

#endif   // _DRIVER_H

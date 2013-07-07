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

#ifndef _NETDB_H
#define _NETDB_H

#include "tree.h"

#include <stdint.h>

typedef uint32_t groupid_t;

#define GROUPID_INVALID UINT32_MAX

typedef struct netdb netdb_t;

typedef void (*netdb_walk_fn_t)(groupid_t, netid_t, unsigned);

netdb_t *netdb_open(tree_t top);
void netdb_close(netdb_t *db);
groupid_t netdb_lookup(netdb_t *db, netid_t nid);
unsigned netdb_size(netdb_t *db);
void netdb_walk(netdb_t *db, netdb_walk_fn_t fn);


#endif  // _NETDB_H

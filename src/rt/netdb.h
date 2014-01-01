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
#include <assert.h>

typedef uint32_t groupid_t;

#define GROUPID_INVALID UINT32_MAX

typedef struct netdb netdb_t;
typedef struct group group_t;

typedef void (*netdb_walk_fn_t)(groupid_t, netid_t, unsigned);

struct netdb {
   group_t   *groups;
   groupid_t *map;
   netid_t    nnets;
   unsigned   max;
};

netdb_t *netdb_open(tree_t top);
void netdb_close(netdb_t *db);
unsigned netdb_size(netdb_t *db);
void netdb_walk(netdb_t *db, netdb_walk_fn_t fn);

static inline groupid_t netdb_lookup(const netdb_t *db, netid_t nid)
{
   assert(nid < db->nnets);
   groupid_t gid = db->map[nid];
   if (likely(gid != GROUPID_INVALID))
      return gid;
   else
      fatal_trace("net %d not in database", nid);
}


#endif  // _NETDB_H

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

#include "netdb.h"
#include "util.h"

#include <stdlib.h>
#include <assert.h>

typedef struct group group_t;

struct group {
   group_t  *next;
   groupid_t gid;
   netid_t   first;
   unsigned  length;
};

struct netdb {
   group_t   *groups;
   groupid_t *map;
   netid_t    nnets;
   unsigned   max;
};

netdb_t *netdb_open(tree_t top)
{
   char name[256];
   snprintf(name, sizeof(name), "_%s.netdb", istr(tree_ident(top)));

   fbuf_t *f = lib_fbuf_open(lib_work(), name, FBUF_IN);
   if (f == NULL)
      fatal("failed to open net database file %s", name);

   netdb_t *db = xmalloc(sizeof(struct netdb));
   db->groups = NULL;
   db->max    = 0;
   db->nnets  = 0;

   groupid_t gid;
   while ((gid = read_u32(f)) != GROUPID_INVALID) {
      group_t *g = xmalloc(sizeof(struct group));
      g->next   = db->groups;
      g->gid    = gid;
      g->first  = read_u32(f);
      g->length = read_u32(f);

      db->groups = g;
      db->max    = MAX(db->max, gid);
      db->nnets  = MAX(db->nnets, g->first + g->length);
   }

   fbuf_close(f);

   db->map = xmalloc(sizeof(groupid_t) * db->nnets);
   for (netid_t i = 0; i < db->nnets; i++)
      db->map[i] = GROUPID_INVALID;

   for (group_t *it = db->groups; it != NULL; it = it->next) {
      for (netid_t i = it->first; i < it->first + it->length; i++)
         db->map[i] = it->gid;
   }

   return db;
}

void netdb_close(netdb_t *db)
{
   while (db->groups != NULL) {
      group_t *next = db->groups->next;
      free(db->groups);
      db->groups = next;
   }

   free(db->map);
   free(db);
}

groupid_t netdb_lookup(netdb_t *db, netid_t nid)
{
   assert(nid < db->nnets);
   groupid_t gid = db->map[nid];
   if (likely(gid != GROUPID_INVALID))
      return gid;
   else
      fatal_trace("net %d not in database", nid);
}

unsigned netdb_size(netdb_t *db)
{
   return db->max + 1;
}

void netdb_walk(netdb_t *db, netdb_walk_fn_t fn)
{
   for (group_t *it = db->groups; it != NULL; it = it->next)
      (*fn)(it->gid, it->first, it->length);
}

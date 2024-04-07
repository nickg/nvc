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

#include "util.h"
#include "fbuf.h"
#include "hash.h"
#include "ucis/ucis-api.h"
#include "ucis/ucis-util.h"
#include "ucis/ucis-structs.h"

#include <assert.h>
#include <string.h>

#define FILE_MAGIC        0x75636973
#define FORMAT_VERSION    1

#define MARKER_HISTORY_NODE 1
#define MARKER_END_OF_FILE  2
#define MARKER_ENTER_SCOPE  3
#define MARKER_LEAVE_SCOPE  4
#define MARKER_COVER_ITEM   5

static void ucis_write_scope(ucisScopeT scope, fbuf_t *f)
{
   write_u8(MARKER_ENTER_SCOPE, f);

   fbuf_put_uint(f, scope->name);
   fbuf_put_uint(f, scope->source);
   fbuf_put_uint(f, scope->type);
   fbuf_put_uint(f, scope->flags);
   fbuf_put_uint(f, scope->numitems);

   for (int i = 0; i < scope->numitems; i++) {
      ucisCoverItemT *ci = ucis_get_cover_item(scope, i);
      write_u8(MARKER_COVER_ITEM, f);
      fbuf_put_uint(f, i);
      fbuf_put_uint(f, ci->data.type);
      fbuf_put_uint(f, ci->data.flags);
   }

   for (ucisScopeT s = scope->children; s != NULL; s = s->next)
      ucis_write_scope(s, f);

   write_u8(MARKER_LEAVE_SCOPE, f);
}

int ucis_Write(ucisT db, const char* file, ucisScopeT scope, int recurse,
               int covertype)
{
   fbuf_t *f = fbuf_open(file, FBUF_OUT, FBUF_CS_NONE);
   if (f == NULL) {
      ucis_error("failed to create %s", file);
      return 1;
   }

   write_u32(FILE_MAGIC, f);
   fbuf_put_uint(f, FORMAT_VERSION);

   const size_t strbytes = tb_len(db->strtab);
   fbuf_put_uint(f, strbytes);
   write_raw(tb_get(db->strtab), strbytes, f);

   for (ucisHistoryNodeT h = db->history_nodes; h != NULL; h = h->next) {
      write_u8(MARKER_HISTORY_NODE, f);
      fbuf_put_uint(f, h->kind);
      fbuf_put_uint(f, h->teststatus);
      write_double(h->simtime, f);
      fbuf_put_uint(f, h->timeunit);
      fbuf_put_uint(f, h->runcwd);
      write_double(h->cputime, f);
      fbuf_put_uint(f, h->seed);
      fbuf_put_uint(f, h->cmd);
      fbuf_put_uint(f, h->args);
      fbuf_put_uint(f, h->compulsory);
      fbuf_put_uint(f, h->date);
      fbuf_put_uint(f, h->username);
      write_double(h->cost, f);
      fbuf_put_uint(f, h->toolcategory);
   }

   for (ucisScopeT s = db->root_scopes; s != NULL; s = s->next)
      ucis_write_scope(s, f);

   write_u8(MARKER_END_OF_FILE, f);

   uint32_t checksum;
   fbuf_close(f, &checksum);

   return 0;
}

int ucis_populate(ucisT db, const char *path)
{
   fbuf_t *f = fbuf_open(path, FBUF_IN, FBUF_CS_NONE);
   if (f == NULL)
      return ucis_error("failed to open %s", path);

   if (read_u32(f) != FILE_MAGIC)
      return ucis_error("database format not recognised");
   else if (fbuf_get_uint(f) != FORMAT_VERSION)
      return ucis_error("incorrect database format version");

   assert(tb_len(db->strtab) == 0);

   const size_t strbytes = fbuf_get_uint(f);
   char *p = tb_reserve(db->strtab, strbytes);
   read_raw(p, strbytes, f);

   ucisStringT pos = 1;
   while (pos < strbytes) {
      shash_put(db->strhash, p + pos, (void *)(uintptr_t)pos);
      pos += strlen(p + pos) + 1;
   }
   assert(pos == strbytes);

   ucisScopeT *scope_tail = &(db->root_scopes);
   ucisHistoryNodeT *history_tail = &(db->history_nodes);
   ucisScopeT scope = NULL;

   uint8_t marker;
   while ((marker = read_u8(f)) != MARKER_END_OF_FILE) {
      switch (marker) {
      case MARKER_HISTORY_NODE:
         {
            ucisHistoryNodeT h = xcalloc(sizeof(struct _ucisHistoryNode));
            h->kind         = fbuf_get_uint(f);
            h->teststatus   = fbuf_get_uint(f);
            h->simtime      = read_double(f);
            h->timeunit     = fbuf_get_uint(f);
            h->runcwd       = fbuf_get_uint(f);
            h->cputime      = read_double(f);
            h->seed         = fbuf_get_uint(f);
            h->cmd          = fbuf_get_uint(f);
            h->args         = fbuf_get_uint(f);
            h->compulsory   = fbuf_get_uint(f);
            h->date         = fbuf_get_uint(f);
            h->username     = fbuf_get_uint(f);
            h->cost         = read_double(f);
            h->toolcategory = fbuf_get_uint(f);

            *history_tail = h;
            history_tail = &(h->next);
         }
         break;
      case MARKER_ENTER_SCOPE:
         {
            ucisScopeT s = ucis_new_object(UCIS_OBJ_SCOPE,
                                           sizeof(struct ucisScopeS));
            s->name     = fbuf_get_uint(f);
            // s->srcinfo
            s->source   = fbuf_get_uint(f);
            s->type     = fbuf_get_uint(f);
            s->flags    = fbuf_get_uint(f);
            // du_scope
            s->numitems = fbuf_get_uint(f);

            if (s->numitems > s->maxitems)
               s->extended = xcalloc_array(s->numitems, sizeof(ucisCoverItemT));

            *scope_tail = s;
            scope_tail = &(s->next);

            scope = s;
         }
         break;
      case MARKER_COVER_ITEM:
         {
            const int index = fbuf_get_uint(f);
            ucisCoverItemT *ci = ucis_get_cover_item(scope, index);
            assert(ci != NULL);

            ci->data.type  = fbuf_get_uint(f);
            ci->data.flags = fbuf_get_uint(f);
         }
         break;
      case MARKER_LEAVE_SCOPE:
         scope = NULL;
         break;
      default:
         fatal_trace("invalid marker %x", marker);
      }
   }

   assert(scope == NULL);

   uint32_t checksum;
   fbuf_close(f, &checksum);

   return 0;
}

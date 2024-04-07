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
#include "diag.h"
#include "hash.h"
#include "ucis/ucis-api.h"
#include "ucis/ucis-structs.h"

#include <assert.h>
#include <string.h>

static ucis_ErrorHandler  error_handler = NULL;
static void              *handler_userdata = NULL;

int ucis_error(const char *fmt, ...)
{
   va_list ap;
   va_start(ap, fmt);

   if (error_handler != NULL) {
      char *buf LOCAL = xvasprintf(fmt, ap);
      ucisErrorT error = {
         .msgstr   = buf,
         .severity = UCIS_MSG_ERROR,
      };
      (*error_handler)(handler_userdata, &error);
   }

   va_end(ap);
   return -1;
}

void ucis_RegisterErrorHandler(ucis_ErrorHandler errHandle, void *userdata)
{
   handler_userdata = userdata;
   error_handler = errHandle;
}

ucisCoverItemT *ucis_get_cover_item(ucisScopeT scope, int index)
{
   if (index < 0 || index >= scope->numitems) {
      ucis_error("invalid cover item index %d", index);
      return NULL;
   }
   else if (scope->maxitems == EMBED_COVER_ITEMS)
      return &(scope->embed[index]);
   else
      return &(scope->extended[index]);
}

bool ucis_is_design_unit(ucisScopeT scope)
{
   switch (scope->type) {
   case UCIS_DU_MODULE:
   case UCIS_DU_ARCH:
   case UCIS_DU_PACKAGE:
   case UCIS_DU_PROGRAM:
   case UCIS_DU_INTERFACE:
      return true;
   default:
      return false;
   }
}

ucisObjT ucis_new_object(ucisObjMaskT kind, size_t size)
{
   assert(size >= sizeof(ucisObjHeaderT));

   ucisObjHeaderT *obj = xcalloc(size);
   obj->kind = kind;

   return obj;
}

ucisStringT ucis_new_string(ucisT db, const char *str)
{
   if (str == NULL)
      return 0;

   const uintptr_t exist = (uintptr_t)shash_get(db->strhash, str);
   if (exist != 0)
      return exist;

   const size_t off = tb_len(db->strtab);
   shash_put(db->strhash, str, (void *)(uintptr_t)off);
   tb_catn(db->strtab, str, strlen(str) + 1);
   return off;
}

const char *ucis_get_string(ucisT db, ucisStringT str)
{
   if (str == 0)
      return NULL;

   assert(str < tb_len(db->strtab));
   return tb_get(db->strtab) + str;
}

ucisScopeT ucis_cast_scope(ucisObjT obj)
{
   if (obj == NULL) {
      ucis_error("object is null");
      return NULL;
   }

   ucisObjHeaderT *header = obj;
   if (header->kind != UCIS_OBJ_SCOPE) {
      ucis_error("object is not a scope");
      return NULL;
   }

   return container_of(header, struct ucisScopeS, header);
}

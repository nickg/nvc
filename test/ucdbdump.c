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
#include "option.h"
#include "thread.h"
#include "ucis/ucis-api.h"

#include <stdio.h>
#include <assert.h>
#include <err.h>
#include <inttypes.h>
#include <string.h>

static void print_coverage_count(ucisCoverDataT* coverdata)
{
   if (coverdata->flags & UCIS_IS_32BIT)
      printf("%d", coverdata->data.int32);
   else if (coverdata->flags & UCIS_IS_64BIT)
      printf("%"PRIi64, coverdata->data.int64);
   else if (coverdata->flags & UCIS_IS_VECTOR) {
      const int bytelen = coverdata->bitlen/8 + (coverdata->bitlen%8)?1:0;
      for (int i = 0; i < bytelen; i++) {
         if (i) printf(" ");
         printf("%02x",coverdata->data.bytevector[i]);
      }
   }
}

static void print_string(ucisT db, ucisObjT obj, ucisStringPropertyEnumT prop)
{
   const char *str = ucis_GetStringProperty(db, obj, -1, prop);
   fputs(str, stdout);
}

static ucisCBReturnT callback(void* userdata, ucisCBDataT* cbdata)
{
   ucisScopeT **underneathp = userdata;

   switch (cbdata->reason) {
   case UCIS_REASON_DU:
      assert(*underneathp == NULL);
      *underneathp = cbdata->obj;
      break;
   case UCIS_REASON_SCOPE:
      break;
   case UCIS_REASON_ENDSCOPE:
      if (cbdata->obj == *underneathp)
         *underneathp = NULL;
      break;
   case UCIS_REASON_CVBIN:
      {
         ucisT db = cbdata->db;
         ucisScopeT scope = (ucisScopeT)cbdata->obj;
         char* name;
         ucisCoverDataT coverdata;
         ucisSourceInfoT sourceinfo;
         ucis_GetCoverData(db, scope, cbdata->coverindex, &name,
                           &coverdata, &sourceinfo);

         if (*underneathp != NULL) {
            // Handle UCIS_INST_ONCE optimisation
            print_string(db, *underneathp, UCIS_STR_SCOPE_NAME);
         }

         print_string(db, scope, UCIS_STR_SCOPE_HIER_NAME);

         if (name != NULL && name[0] != '\0')
            printf("%c%s: ", ucis_GetPathSeparator(db), name);
         else
            printf(" [%s:%d]: ", ucis_GetFileName(db, sourceinfo.filehandle),
                   sourceinfo.line);

         print_coverage_count(&coverdata);
         printf("\n");
      }
      break;
   default:
      break;
   }

   return UCIS_SCAN_CONTINUE;
}

static void ucis_error_handler(void *data, ucisErrorT *errorInfo)
{
   errx(1, "%s", errorInfo->msgstr);
}

int main(int argc, char **argv)
{
#ifndef HAVE_UCIS_H
   term_init();
   thread_init();
   set_default_options();
   register_signal_handlers();
#endif

   ucis_RegisterErrorHandler(ucis_error_handler, NULL);

   if (argc != 2)
      errx(1, "usage: ucdbdump FILE");

   ucisT db = ucis_Open(argv[1]);
   if (db == NULL)
      return 1;

   ucisScopeT *underneath = NULL;
   ucis_CallBack(db, NULL, callback, &underneath);
   assert(underneath == NULL);

   ucis_Close(db);
   return 0;
}

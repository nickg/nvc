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
#include "ucis/ucis-api.h"
#include "ucis/ucis-util.h"

#include <stdio.h>

static void ucis_write_xml(ucisT db, FILE *f, time_t date)
{
   LOCAL_TEXT_BUF tb = tb_new();
   tb_strftime(tb, "L%FT%TZ", date);

   fprintf(f, "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n");
   fprintf(f, "<UCIS xmlns:ucis=\"http://www.w3.org/2001/XMLSchema-instance\""
           " writtenBy=\""PACKAGE_STRING"\" writtenTime=\"%s\""
           " ucisVersion=\"1.0\">\n", tb_get(tb));

   fprintf(f, "</UCIS>\n");
}

int ucis_WriteToInterchangeFormat(ucisT db, const char* file, ucisScopeT scope,
                                  int recurse, int covertype)
{
   FILE *f = fopen(file, "w");
   if (f == NULL) {
      // ucis_error()
      return -1;
   }

   ucis_write_xml(db, f, time(NULL));

   fclose(f);
   return 0;
}

//
//  Copyright (C) 2013-2022  Nick Gasson
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

#include "rt.h"
#include "util.h"
#include "tree.h"
#include "array.h"

#include <string.h>

typedef struct {
   char  *text;
   size_t len;
} glob_t;

typedef A(glob_t) glob_array_t;

static glob_array_t incl;
static glob_array_t excl;

void wave_include_glob(const char *glob)
{
   APUSH(incl, ((glob_t){ .text = strdup(glob), .len = strlen(glob) }));
}

void wave_exclude_glob(const char *glob)
{
   APUSH(excl, ((glob_t){ .text = strdup(glob), .len = strlen(glob) }));
}

static void wave_process_file(const char *fname, bool include)
{
   FILE *f = fopen(fname, "r");
   if (f == NULL)
      return;

   notef("%s signals from %s", include ? "including" : "excluding", fname);

   int lineno = 0;
   char line[1024];
   while (!feof(f) && (lineno++, fgets(line, sizeof(line), f) != NULL)) {
      // Erase comments
      bool comment = false;
      for (char *p = line; *p != '\0'; p++) {
         if (*p == '#')
            comment = true;
         if (comment || (*p == '\r') || (*p == '\n'))
            *p = '\0';
      }

      char glob[1024];
      if (sscanf(line, " %1023s ", glob) == 1) {
         if (include)
            wave_include_glob(glob);
         else
            wave_exclude_glob(glob);
      }
   }

   fclose(f);
}

void wave_include_file(const char *base)
{
   char *inclf LOCAL = xasprintf("%s.include", base);
   wave_process_file(inclf, true);

   char *exclf LOCAL = xasprintf("%s.exclude", base);
   wave_process_file(exclf, false);
}

bool wave_should_dump(ident_t name)
{
   for (int i = 0; i < excl.count; i++) {
      if (ident_glob(name, excl.items[i].text, excl.items[i].len))
         return false;
   }

   for (int i = 0; i < incl.count; i++) {
      if (ident_glob(name, incl.items[i].text, incl.items[i].len))
         return true;
   }

   return (incl.count == 0);
}

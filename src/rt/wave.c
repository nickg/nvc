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

#include "rt.h"
#include "util.h"
#include "tree.h"

#include <string.h>

typedef struct {
   char  *text;
   size_t len;
} glob_t;

static int     n_incl = 0;
static int     incl_sz = 0;
static int     n_excl = 0;
static int     excl_sz = 0;
static glob_t *incl;
static glob_t *excl;

void wave_include_glob(const char *glob)
{
   if (n_incl == incl_sz) {
      incl_sz = MAX(incl_sz * 2, 256);
      incl = xrealloc(incl, incl_sz * sizeof(glob_t));
   }

   incl[n_incl].text = strdup(glob);
   incl[n_incl].len  = strlen(glob);

   n_incl++;
}

void wave_exclude_glob(const char *glob)
{
   if (n_excl == excl_sz) {
      excl_sz = MAX(excl_sz * 2, 256);
      excl = xrealloc(excl, excl_sz * sizeof(glob_t));
   }

   excl[n_excl].text = strdup(glob);
   excl[n_excl].len  = strlen(glob);

   n_excl++;
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
   char buf[256];

   checked_sprintf(buf, sizeof(buf), "%s.include", base);
   wave_process_file(buf, true);

   checked_sprintf(buf, sizeof(buf), "%s.exclude", base);
   wave_process_file(buf, false);
}

bool wave_should_dump(ident_t name)
{
   for (int i = 0; i < n_excl; i++) {
      if (ident_glob(name, excl[i].text, excl[i].len))
         return false;
   }

   for (int i = 0; i < n_incl; i++) {
      if (ident_glob(name, incl[i].text, incl[i].len))
         return true;
   }

   return (n_incl == 0);
}

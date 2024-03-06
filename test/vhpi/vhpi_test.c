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

#include "vhpi_test.h"

#include <stdlib.h>
#include <string.h>

typedef struct {
   const char *name;
   void (*startup)();
} vhpi_test_t;

static const vhpi_test_t tests[] = {
   { "vhpi1",    vhpi1_startup },
   { "vhpi2",    vhpi2_startup },
   { "vhpi3",    vhpi3_startup },
   { "vhpi4",    NULL },
   { "vhpi5",    vhpi5_startup },
   { "issue612", NULL },
   { "vhpi6",    vhpi6_startup },
   { "vhpi7",    vhpi7_startup },
   { "vhpi8",    vhpi8_startup },
   { "vhpi9",    vhpi9_startup },
   { "issue744", issue744_startup },
   { "vhpi10",   vhpi10_startup },
   { "vhpi11",   vhpi11_startup },
   { "issue762", issue762_startup },
   { "vhpi12",   vhpi12_startup },
   { NULL,       NULL },
};

void __check_error(const char *file, int lineno)
{
   vhpiErrorInfoT info;
   if (vhpi_check_error(&info))
      vhpi_assert(vhpiFailure, "%s:%d: unexpected error '%s'",
                  file, lineno, info.message);
}

static void shared_startup(void)
{
   const char *test_name = getenv("TEST_NAME");
   if (test_name == NULL)
      vhpi_assert(vhpiFailure, "TEST_NAME environment variable not set");

   vhpi_printf("test name is %s", test_name);

   for (const vhpi_test_t *p = tests; p->name; p++) {
      if (strcmp(p->name, test_name) == 0) {
         if (p->startup != NULL)
            (*p->startup)();
         return;
      }
   }

   vhpi_assert(vhpiFailure, "unknown test %s", test_name);
}

void (*vhpi_startup_routines[])() = {
   shared_startup,
   NULL
};

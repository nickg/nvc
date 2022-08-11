//
//  Copyright (C) 2022  Nick Gasson
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

#include "opt.h"
#include "util.h"

#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <ctype.h>

typedef enum {
   OPT_KIND_UNDEF,
   OPT_KIND_INT,
   OPT_KIND_STRING
} option_kind_t;

typedef union {
   int   i;
   char *s;
} optval_t;

typedef struct {
   option_kind_t kind;
   optval_t      value;
} option_t;

static option_t options[OPT_LAST_NAME];

static void opt_set_generic(opt_name_t name, option_kind_t kind,
                            optval_t value)
{
   assert(name < OPT_LAST_NAME);

   option_t *o = &(options[name]);
   o->value = value;
   o->kind  = kind;
}

static optval_t opt_get_generic(opt_name_t name, option_kind_t kind)
{
   assert(name < OPT_LAST_NAME);

   const option_t *o = &(options[name]);
   if (o->kind == kind)
      return o->value;
   else if (o->kind == OPT_KIND_UNDEF)
      fatal_trace("initial value for option %d not set", name);
   else
      fatal_trace("wrong option kind for %d", name);
}

void opt_set_int(opt_name_t name, int val)
{
   opt_set_generic(name, OPT_KIND_INT, (optval_t)val);
}

int opt_get_int(opt_name_t name)
{
   return opt_get_generic(name, OPT_KIND_INT).i;
}

void opt_set_str(opt_name_t name, const char *val)
{
   opt_set_generic(name, OPT_KIND_STRING, (optval_t)(val ? strdup(val) : NULL));
}

const char *opt_get_str(opt_name_t name)
{
   return opt_get_generic(name, OPT_KIND_STRING).s;
}

bool opt_get_verbose(opt_name_t name, const char *filter)
{
   const char *value = opt_get_str(name);
   if (value == NULL || *value == '\0')
      return false;
   else if (isdigit((int)*value))
      return true;
   else if (filter == NULL)
      return false;
   else if (value[0] == '^')
      return strcmp(value + 1, filter) == 0;
   else
      return strstr(filter, value) != NULL;
}

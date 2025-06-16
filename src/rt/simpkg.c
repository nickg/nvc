//
//  Copyright (C) 2022-2023  Nick Gasson
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
#include "jit/jit.h"
#include "jit/jit-ffi.h"
#include "rt/model.h"
#include "rt/rt.h"

DLLEXPORT
void _nvc_ieee_warnings(jit_scalar_t *args)
{
   args[0].integer = (opt_get_int(OPT_IEEE_WARNINGS) == IEEE_WARNINGS_ON);
}

DLLEXPORT
void _nvc_current_delta(jit_scalar_t *args)
{
   rt_model_t *m = get_model_or_null();

   if (m == NULL)
      args[0].integer = 0;
   else {
      unsigned delta;
      model_now(m, &delta);

      args[0].integer = delta;
   }
}

void _nvc_sim_pkg_init(void)
{
   // Dummy function to force linking
}

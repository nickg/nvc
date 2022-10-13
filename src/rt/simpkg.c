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

#include "util.h"
#include "opt.h"
#include "jit/jit.h"
#include "jit/jit-exits.h"
#include "jit/jit-ffi.h"
#include "rt/rt.h"

DLLEXPORT
bool _nvc_ieee_warnings(void)
{
   return opt_get_int(OPT_IEEE_WARNINGS);
}

DLLEXPORT
int _nvc_current_delta(void)
{
   return x_current_delta();
}

void _nvc_sim_pkg_init(void)
{
   // Dummy function to force linking
}

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

#include "stdlib.h"
#include "jit/jit.h"
#include "rt/model.h"

void x_get_random(jit_scalar_t *result)
{
    rt_model_t *m = get_model_or_null();

    // TODO: Would need to be handled when randomizer is called from
    //       elaboration (e.g. vlog param initialied by $random)
    if (m == NULL)
        return;

    int64_t rnd_val = model_get_random_number(m);

    result[0].integer = rnd_val;
}

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

#include "util.h"
#include "option.h"
#include "psl/psl-fsm.h"
#include "psl/psl-node.h"
#include "psl/psl-phase.h"
#include "vcode.h"

#include <assert.h>

void psl_lower(lower_unit_t *parent, psl_node_t p, ident_t name)
{
   assert(psl_kind(p) == P_ASSERT);

   psl_fsm_t *fsm = psl_fsm_new(psl_value(p));

   if (opt_get_verbose(OPT_PSL_VERBOSE, istr(name))) {
      char *fname LOCAL = xasprintf("%s.dot", istr(name));
      psl_fsm_dump(fsm, fname);
   }

   psl_fsm_free(fsm);
}

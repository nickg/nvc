//
//  Copyright (C) 2022-2024  Nick Gasson
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

#include "psl/psl-node.h"

bool psl_is_directive(psl_node_t p)
{
   psl_kind_t kind = psl_kind(p);

   if (kind == P_ASSERT || kind == P_ASSUME ||
       kind == P_NEVER || kind == P_COVER)
      return true;

   return false;
}
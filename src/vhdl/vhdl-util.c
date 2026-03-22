//
//  Copyright (C) 2025  Nick Gasson
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
#include "common.h"
#include "tree.h"
#include "type.h"
#include "vhdl/vhdl-util.h"

ident_t predef_func_name(type_t type, const char *op)
{
   type_t base = type_base_recur(type);

   LOCAL_TEXT_BUF tb = tb_new();
   tb_printf(tb, "%s.\"%s\"(", istr(ident_runtil(type_ident(base), '.')), op);
   mangle_one_type(tb, base);
   mangle_one_type(tb, base);
   tb_cat(tb, ")");
   mangle_one_type(tb, std_type(NULL, STD_BOOLEAN));
   tb_cat(tb, "$predef");

   return ident_new(tb_get(tb));
}

bool vhdl_is_short_circuit(subprogram_kind_t kind)
{
   return kind == S_SCALAR_AND || kind == S_SCALAR_OR
      || kind == S_SCALAR_NOR || kind == S_SCALAR_NAND;
}

bool vhdl_is_logical(subprogram_kind_t kind)
{
   switch (kind) {
   case S_SCALAR_EQ:
   case S_SCALAR_NEQ:
   case S_SCALAR_LT:
   case S_SCALAR_LE:
   case S_SCALAR_GE:
   case S_SCALAR_GT:
   case S_SCALAR_OR:
   case S_SCALAR_NOR:
   case S_SCALAR_AND:
   case S_SCALAR_NAND:
   case S_SCALAR_XOR:
   case S_SCALAR_XNOR:
   case S_SCALAR_NOT:
   case S_IEEE_OR:
   case S_IEEE_NOR:
   case S_IEEE_AND:
   case S_IEEE_NAND:
   case S_IEEE_XOR:
   case S_IEEE_XNOR:
      return true;
   default:
      return false;
   }
}

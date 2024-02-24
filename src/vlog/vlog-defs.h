//
//  Copyright (C) 2024 Nick Gasson
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

#ifndef _VLOG_DEFS_H
#define _VLOG_DEFS_H

#define T_LOGIC "19NVC.VERILOG.T_LOGIC"
#define T_PACKED_LOGIC "26NVC.VERILOG.T_PACKED_LOGIC"
#define T_INT64 "19NVC.VERILOG.T_INT64"
#define T_NET_VALUE "23NVC.VERILOG.T_NET_VALUE"
#define T_NET_ARRAY "23NVC.VERILOG.T_NET_ARRAY"

typedef enum {
   _X = 0, _SUPPLY0, _STRONG0, _PULL0, _LARGE0, _WEAK0, _MEDIUM0,
   _SMALL0, _HIGHZ0, _HIGHZ1, _SMALL1, _MEDIUM1, _WEAK1, _LARGE1,
   _PULL1, _STRONG1, _SUPPLY1
} net_value_t;

#endif  // _VLOG_DEFS_H

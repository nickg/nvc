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

#ifndef _RT_PRINTER_H
#define _RT_PRINTER_H

#include "prim.h"

typedef struct _print_func print_func_t;

#define PRINT_F_RADIX 0x3
#define PRINT_F_BIN 1
#define PRINT_F_HEX 2
#define PRINT_F_DEC 3
typedef uint32_t print_flags_t;

printer_t *printer_new(void);
void printer_free(printer_t *p);
print_func_t *printer_for(printer_t *p, type_t type);

const char *print_signal(print_func_t *fn, rt_signal_t *s, print_flags_t flags);

#endif  // _RT_PRINTER_H

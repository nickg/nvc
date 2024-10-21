//
//  Copyright (C) 2023-2024  Nick Gasson
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

#ifndef _VHPI_TEST_H
#define _VHPI_TEST_H

#include "vhpi_user.h"

#define fail_if(x)                                                      \
   if (x) vhpi_assert(vhpiFailure, "assertion '%s' failed at %s:%d",    \
                      #x, __FILE__, __LINE__)
#define fail_unless(x) fail_if(!(x))

#define check_error() __check_error(__FILE__, __LINE__)
#define check_handle(h) __check_handle(h, __FILE__, __LINE__)

void __check_error(const char *file, int lineno);
void __check_handle(vhpiHandleT h, const char *file, int lineno);

void vhpi1_startup(void);
void vhpi2_startup(void);
void vhpi3_startup(void);
void vhpi5_startup(void);
void vhpi6_startup(void);
void vhpi7_startup(void);
void vhpi8_startup(void);
void vhpi9_startup(void);
void vhpi10_startup(void);
void vhpi11_startup(void);
void vhpi12_startup(void);
void vhpi13_startup(void);
void vhpi14_startup(void);
void vhpi15_startup(void);
void issue744_startup(void);
void issue762_startup(void);
void issue978_startup(void);
void issue988_startup(void);
void issue1035_startup(void);

#endif  // _VHPI_TEST_H

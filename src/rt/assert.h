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

#ifndef _RT_ASSERT_H
#define _RT_ASSERT_H

#include "prim.h"
#include "diag.h"

typedef enum {
   SEVERITY_NOTE = 0,
   SEVERITY_WARNING = 1,
   SEVERITY_ERROR = 2,
   SEVERITY_FAILURE = 3
} vhdl_severity_t;

diag_level_t vhdl_to_diag_severity(vhdl_severity_t severity);

vhdl_severity_t set_exit_severity(vhdl_severity_t severity);
void set_stderr_severity(vhdl_severity_t severity);

int64_t get_vhdl_assert_count(vhdl_severity_t severity);
void clear_vhdl_assert(void);
void set_vhdl_assert_enable(vhdl_severity_t severity, bool enable);
bool get_vhdl_assert_enable(vhdl_severity_t severity);

#endif   // _RT_ASSERT_H

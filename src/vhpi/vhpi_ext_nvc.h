//
//  Copyright (C) 2026  Nick Gasson
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

#ifndef VHPI_EXT_NVC_H
#define VHPI_EXT_NVC_H

#ifdef VHPI_USER_H
#error This file must be included before vhpi_user.h
#endif

#define VHPIEXTEND_CLASSES ,                    \
   vhpiVerilogModuleK = 2001

#define VHPIEXTEND_INT_PROPERTIES ,             \
   vhpiRandomSeedP = 1100

#endif  // VHPI_EXT_NVC_H

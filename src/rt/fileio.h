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

#ifndef _RT_FILEIO_H
#define _RT_FILEIO_H

#include "prim.h"

typedef enum {
   FILE_READ = 0,
   FILE_WRITE = 1,
   FILE_APPEND = 2,
   FILE_READ_WRITE = 3,
} file_mode_t;

typedef uint32_t file_handle_t;

bool file_mode(file_handle_t fh, file_mode_t *mode);
bool file_logical_name(file_handle_t fh, const char **name);

#endif  // _RT_FILEIO_H

//
//  Copyright (C) 2013-2022  Nick Gasson
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

#ifndef _RT_WAVE_H
#define _RT_WAVE_H

#include "prim.h"

typedef enum {
   WAVE_FORMAT_FST,
   WAVE_FORMAT_VCD
} wave_format_t;

wave_dumper_t *wave_dumper_new(const char *file, const char *gtkw_file,
                               tree_t top, wave_format_t format);
void wave_dumper_free(wave_dumper_t *wd);
void wave_dumper_restart(wave_dumper_t *wd, rt_model_t *m);

void wave_include_glob(const char *glob);
void wave_exclude_glob(const char *glob);
void wave_include_file(const char *base);

#endif  // _RT_WAVE_H

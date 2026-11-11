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

#ifndef _RT_WAVE_LIVE_H
#define _RT_WAVE_LIVE_H

#include "fstapi.h"

// Append-only text waveform stream that can be read while the simulation
// is still running (FST files cannot: the hierarchy is only written when
// the file is closed).  The entry points mirror the fstWriter functions
// used by the wave dumper so wave.c can dispatch to either backend.
//
// Format, one record per line:
//   LVW 1                              header
//   TS <exp>                           time unit is 10^exp seconds
//   S <scope type> <name>              enter scope
//   U                                  leave scope
//   V <handle> <var type> <width> <kind> <name>
//                                      kind is L (logic), R (real), S (string);
//                                      a handle seen twice is an alias
//   E                                  end of definitions
//   #<time>                            following values happen at this time
//   <handle> L <bits>                  also: <handle> R <double>
//                                      and <handle> S <text>

typedef struct _lvw lvw_t;

lvw_t *lvw_create(const char *path);
void lvw_flush(lvw_t *lw);
void lvw_close(lvw_t *lw);

void lvw_SetTimescale(lvw_t *lw, int exponent);
void lvw_SetScope(lvw_t *lw, enum fstScopeType type, const char *name,
                  const char *component);
void lvw_SetUpscope(lvw_t *lw);
void lvw_SetAttrEnd(lvw_t *lw);
void lvw_SetSourceStem(lvw_t *lw, const char *path, unsigned int line,
                       unsigned int use_realpath);
fstHandle lvw_CreateVar2(lvw_t *lw, enum fstVarType vt, enum fstVarDir vd,
                         uint32_t len, const char *name, fstHandle alias,
                         const char *type, enum fstSupplementalVarType svt,
                         enum fstSupplementalDataType sdt);
fstEnumHandle lvw_CreateEnumTable(lvw_t *lw, const char *name,
                                  uint32_t elem_count, unsigned int min_valbits,
                                  const char **literal_arr,
                                  const char **val_arr);
void lvw_EmitEnumTableRef(lvw_t *lw, fstEnumHandle handle);
void lvw_EmitTimeChange(lvw_t *lw, uint64_t tim);
void lvw_EmitValueChange(lvw_t *lw, fstHandle handle, const void *val);
void lvw_EmitVariableLengthValueChange(lvw_t *lw, fstHandle handle,
                                       const void *val, uint32_t len);

#endif  // _RT_WAVE_LIVE_H

//
//  Copyright (C) 2013  Nick Gasson
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

#ifndef _FBUF_H
#define _FBUF_H

#include "util.h"

//
// Compressed binary file input/output
//

typedef struct fbuf fbuf_t;

typedef enum {
   FBUF_IN,
   FBUF_OUT,
} fbuf_mode_t;

fbuf_t *fbuf_open(const char *file, fbuf_mode_t mode);
void fbuf_close(fbuf_t *f);

// TODO: delete these?
long fbuf_tell(fbuf_t *f);
void fbuf_seek(fbuf_t *f, long offset, int whence);
void fbuf_flush(fbuf_t *f);

void write_u32(uint32_t u, fbuf_t *f);
void write_u16(uint16_t s, fbuf_t *f);
bool write_b(bool b, fbuf_t *f);
void write_i32(int32_t i, fbuf_t *f);
void write_i64(int64_t i, fbuf_t *f);
void write_u8(uint8_t u, fbuf_t *f);
void write_raw(const void *buf, size_t len, fbuf_t *f);

uint32_t read_u32(fbuf_t *f);
uint16_t read_u16(fbuf_t *f);
bool read_b(fbuf_t *f);
int32_t read_i32(fbuf_t *f);
int64_t read_i64(fbuf_t *f);
uint8_t read_u8(fbuf_t *f);
void read_raw(void *buf, size_t len, fbuf_t *f);

#endif  // _FBUF_H

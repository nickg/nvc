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

#ifndef _RT_COPY_H
#define _RT_COPY_H

#include "prim.h"

void _copy2(void *p1, void *p2, const void *src, size_t len);

__attribute__((always_inline))
static inline void copy2(void *p1, void *p2, const void *src, size_t len)
{
   if (len == 1) {
      *(unsigned char *)p1 = *(unsigned char *)p2;
      *(unsigned char *)p2 = *(const unsigned char *)src;
   }
   else
      return _copy2(p1, p2, src, len);
}

bool _cmp_bytes(const void *a, const void *b, size_t size)
   __attribute__((pure));

__attribute__((always_inline))
static inline bool cmp_bytes(const void *a, const void *b, size_t size)
{
   if (size == 1)
      return *(unsigned char *)a == *(unsigned char *)b;
   else
      return _cmp_bytes(a, b, size);
}

#endif   // _RT_COPY_H

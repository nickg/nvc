//
//  Copyright (C) 2022-2024  Nick Gasson
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

#ifndef _VPI_MACROS_H
#define _VPI_MACROS_H

#define DEF_CLASS(class, tag, path)                             \
   static UNUSED c_##class *is_##class(c_vpiObject *obj)        \
   {                                                            \
      if (obj->type == tag)                                     \
         return container_of(obj, c_##class, path);             \
      else                                                      \
         return NULL;                                           \
   }                                                            \
                                                                \
   static UNUSED c_##class *cast_##class(c_vpiObject *obj)      \
   {                                                            \
      c_##class *p = is_##class(obj);                           \
      if (p == NULL)                                            \
         vpi_error(vpiError, NULL, "object type %s is not a "   \
                   #tag, vpi_type_str(obj->type));              \
      return p;                                                 \
   }

#define VPI_MISSING fatal_trace("VPI function %s not implemented", __func__)

#define VPI_TRACE(...) do {                             \
      extern int _vhpi_trace_on(void);                  \
      if (unlikely(_vhpi_trace_on()))                   \
         vpi_trace(__func__, __VA_ARGS__);              \
   } while (0)

#endif  // _VPI_MACROS_H

//
//  Copyright (C) 2011  Nick Gasson
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

#ifndef INC_RT_SIGNAL_H
#define INC_RT_SIGNAL_H

// XXX: this file is deprecated

// Fixed fields in signal structure
enum {
   SIGNAL_RESOLVED,
   SIGNAL_LAST_VALUE,
   SIGNAL_DECL,
   SIGNAL_FLAGS,
   SIGNAL_N_SOURCES,
   SIGNAL_OFFSET,
   SIGNAL_SOURCES,
   SIGNAL_SENSITIVE,
   SIGNAL_EVENT_CB,
   SIGNAL_RESOLUTION,
   SIGNAL_LAST_EVENT,

   SIGNAL_N_FIELDS
};

#endif // RT_SIGNAL_H

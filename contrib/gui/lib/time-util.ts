//
//  Copyright (C) 2023  Nick Gasson
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

export function formatTime(time: bigint) {
  if (time % 1000000n == 0n) {
    return (time / 1000000n).toString() + " ns";
  }
  else if (time % 1000n == 0n) {
    return (time / 1000n).toString() + " ps";
  }
  else {
    return time.toString() + " fs";
  }
}

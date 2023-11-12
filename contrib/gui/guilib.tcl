#
#  Copyright (C) 2023  Nick Gasson
#
#  This program is free software: you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation, either version 3 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  You should have received a copy of the GNU General Public License
#  along with this program.  If not, see <http://www.gnu.org/licenses/>.
#

namespace eval ::nvc {

  proc send {text} {
    puts -nonewline backchannel $text
  }

  proc resultToJSON {name code} {
    send [string cat "\{ " {"command": } "\"" $name "\", " {"result": }]
    uplevel 1 $code
    send "\}\n"
    flush backchannel
  }

  proc listToJSON {var items code} {
    upvar 1 $var elem
    variable needComma 0
    send "\["
    foreach elem $items {
      if $needComma {
        send ", "
      }
      set needComma 1
      uplevel 1 $code
    }
    send "\]"
  }

  proc dictToJSON {dict} {
    variable needComma 0
    send "\{"
    dict for {key value} $dict {
      if $needComma {
        send ", "
      }
      set needComma 1
      send [string cat {"} $key {": "} $value {"}]
    }
    send "\}"
  }

  proc RefreshRegions {} {
    resultToJSON RefreshRegions {
      listToJSON r [describe {*}[find regions -r *]] {
        dictToJSON $r
      }
    }
  }

}

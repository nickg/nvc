-------------------------------------------------------------------------------
--  Copyright (C) 2022  Nick Gasson
--
--  This program is free software; you can redistribute it and/or modify
--  it under the terms of the GNU Lesser General Public License as
--  published by the Free Software Foundation; either version 3 of the
--  License, or (at your option) any later version.
--
--  This program is distributed in the hope that it will be useful, but
--  WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
--  Lesser General Public License for more details.
--
--  You should have received a copy of the GNU Lesser General Public License
--  along with this program; if not, see <http://www.gnu.org/licenses/>.
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- The compiler generates calls to subprograms in this package to implement
-- certain predefined IEEE.STD_LOGIC_1164 operations.  Do not call these
-- subprograms directly.
-------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;

package ieee_support is

    -- Raise an error if VAL contains a '-'
    procedure check_match_expression (val : std_ulogic_vector);
    procedure check_match_expression (val : std_ulogic);

    -- Matching relational operators
    function rel_match_eq (l, r : std_ulogic) return std_ulogic;
    function rel_match_lt (l, r : std_ulogic) return std_ulogic;
    function rel_match_leq (l, r : std_ulogic) return std_ulogic;

end package;

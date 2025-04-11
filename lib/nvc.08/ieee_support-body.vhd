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

package body ieee_support is

    procedure check_match_expression (val : std_ulogic_vector) is
    begin
        for i in val'range loop
            assert val(i) /= '-' report "value of matching case statement expression """
                & to_string(val) & """ contains a '-'";
        end loop;
    end procedure;

    procedure check_match_expression (val : std_ulogic) is
    begin
        assert val /= '-' report "value of matching case statement expression is '-'";
    end procedure;

end package body;

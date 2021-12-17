-------------------------------------------------------------------------------
--  Copyright (C) 2021  Nick Gasson
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

package body polyfill is

    function to_string (value : real; spec : string) return string is
        function impl (value : real; spec : string) return string;
        attribute foreign of impl : function is "_std_to_string_real_format";
    begin
        return impl(value, spec);
    end function;

    function to_hstring (value : bit_vector) return string is
        function impl (value : bit_vector) return string;
        attribute foreign of impl : function is "_std_to_hstring_bit_vec";
    begin
        return impl(value);
    end function;

    function to_ostring (value : bit_vector) return string is
        function impl (value : bit_vector) return string;
        attribute foreign of impl : function is "_std_to_ostring_bit_vec";
    begin
        return impl(value);
    end function;

    function maximum (x, y : integer) return integer is
    begin
        if x > y then
            return x;
        else
            return y;
        end if;
    end function;

    function minimum (x, y : integer) return integer is
    begin
        if x < y then
            return x;
        else
            return y;
        end if;
    end function;

end package body;

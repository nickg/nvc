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

-------------------------------------------------------------------------------
-- This package provides VHDL interfaces to internal simulator
-- functionality.
-------------------------------------------------------------------------------

package sim_pkg is

    -- Return TRUE if IEEE warning messages should be displayed
    function ieee_warnings return boolean;

    attribute foreign of ieee_warnings : function is "_nvc_ieee_warnings";

end package;

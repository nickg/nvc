-------------------------------------------------------------------------------
--  Copyright (C) 2021-2022  Nick Gasson
--
--  Licensed under the Apache License, Version 2.0 (the "License");
--  you may not use this file except in compliance with the License.
--  You may obtain a copy of the License at
--
--     http://www.apache.org/licenses/LICENSE-2.0
--
--  Unless required by applicable law or agreed to in writing, software
--  distributed under the License is distributed on an "AS IS" BASIS,
--  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
--  See the License for the specific language governing permissions and
--  limitations under the License.
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- This package provides VHDL interfaces to internal simulator
-- functionality.
-------------------------------------------------------------------------------

package sim_pkg is

    -- Return TRUE if IEEE warning messages should be displayed
    function ieee_warnings return boolean;

    attribute foreign of ieee_warnings : function is "INTERNAL _nvc_ieee_warnings";

    -- Return the current delta cycle number
    impure function current_delta_cycle return natural;

    attribute foreign of current_delta_cycle : function is "INTERNAL _nvc_current_delta";

end package;

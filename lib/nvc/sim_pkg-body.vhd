-------------------------------------------------------------------------------
--  Copyright (C) 2024  Nick Gasson
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

package body sim_pkg is

    impure function init_ieee_no_warning return boolean is
        function get_option return boolean is
        begin
            return false;                -- Has a foreign implementation
        end function;

        attribute foreign of get_option : function is "INTERNAL _nvc_ieee_warnings";
    begin
        return not get_option;
    end function;

    constant ieee_no_warning : boolean := init_ieee_no_warning;

    impure function current_delta_cycle return natural is
    begin
        return 0;                       -- Has a foreign implementation
    end function;

end package body;

-------------------------------------------------------------------------------
--  Copyright (C) 2021-2024  Nick Gasson
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

package body polyfill is

    function to_string (value : integer) return string is
    begin
        return integer'image(value);
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

-------------------------------------------------------------------------------
--  Copyright (C) 2023  Nick Gasson
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

package body verilog is

    function to_integer (value : t_packed_logic) return integer is
    begin
        return 0;
    end function;

    function to_time (value : t_packed_logic) return delay_length is
        alias v      : t_packed_logic(0 to value'length - 1) is value;
        variable r   : delay_length := 0 fs;
        variable add : delay_length := 1 fs;
    begin
        for i in v'reverse_range loop
            if v(i) = '1' then
                r := r + add;
            end if;
            add := add * 2;
        end loop;
        return r;
    end function;

end package body;

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

use work.polyfill.all;

package body verilog is

    function to_integer (value : t_packed_logic) return t_int64 is
        alias v      : t_packed_logic(0 to value'length - 1) is value;
        variable r   : t_int64 := 0;
        variable add : t_int64 := 1;
    begin
        for i in v'range loop
            if v(i) = '1' then
                r := r + add;
            end if;
            add := add * 2;
        end loop;
        return r;
    end function;

    function to_time (value : t_packed_logic) return delay_length is
        alias v      : t_packed_logic(0 to value'length - 1) is value;
        variable r   : delay_length := 0 fs;
        variable add : delay_length := 1 fs;
    begin
        for i in v'range loop
            if v(i) = '1' then
                r := r + add;
            end if;
            add := add * 2;
        end loop;
        return r;
    end function;

    function to_vhdl (value : t_logic) return std_ulogic is
    begin
        case value is
            when '1' => return '1';
            when '0' => return '0';
            when 'Z' => return 'Z';
            when others => return 'U';
        end case;
    end function;

    function to_verilog (value : std_ulogic) return t_logic is
    begin
        case value is
            when '1' | 'H' => return '1';
            when '0' | 'L' => return '0';
            when 'Z' => return 'Z';
            when others => return 'X';
        end case;
    end function;

    function "and" (l, r : t_logic) return t_logic is
    begin
        if l = '1' and r = '1' then
            return '1';
        elsif l = 'X' or r = 'X' or l = 'Z' or r = 'Z' then
            return 'X';
        else
            return '0';
        end if;
    end function;

    function "and" (l, r : t_packed_logic) return t_packed_logic is
        constant llen   : natural := l'length;
        constant rlen   : natural := r'length;
        constant len    : natural := maximum(llen, rlen);
        alias la        : t_packed_logic(0 to llen - 1) is l;
        alias ra        : t_packed_logic(0 to rlen - 1) is r;
        variable result : t_packed_logic(0 to len - 1);
    begin
        for i in result'range loop
            if i < llen and i < rlen then
                result(i) := la(i) and ra(i);
            else
                result(i) := '0';
            end if;
        end loop;
        return result;
    end function;

end package body;

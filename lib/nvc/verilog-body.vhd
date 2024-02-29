-------------------------------------------------------------------------------
--  Copyright (C) 2023-2024  Nick Gasson
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

    subtype t_strength is natural range 0 to 7;

    function strength (value : t_net_value) return t_strength is
    begin
        case value is
            when highz0 | highz1 => return 0;
            when small0 | small1 => return 1;
            when medium0 | medium1 => return 2;
            when weak0 | weak1 => return 3;
            when large0 | large1 => return 4;
            when pull0 | pull1 => return 5;
            when strong0 | strong1 => return 6;
            when supply0 | supply1 | 'X' => return 7;
        end case;
    end function;

    function resolved (inputs : t_net_array) return t_net_value is
        variable result : t_net_value;
        constant count  : natural := inputs'length;
        alias a_inputs  : t_net_array(1 to count) is inputs;
    begin
        if inputs'length = 0 then
            return 'X';
        else
            result := a_inputs(1);
            for i in 2 to count loop
                if strength(a_inputs(i)) > strength(result) then
                    result := a_inputs(i);
                end if;
            end loop;
            return result;
        end if;
    end function;

    function to_logic (value : t_net_value) return t_logic is
    begin
        case value is
            when 'X' =>
                return 'X';
            when supply0 | strong0 | pull0 | large0 | weak0
                | medium0 | small0 =>
                return '0';
            when small1 | medium1 | weak1 | large1 | pull1
                | strong1 | supply1 =>
                return '1';
            when highz1 | highz0 =>
                return 'Z';
        end case;
    end function;

    function to_logic (value : t_net_array) return t_packed_logic is
        constant length : natural := value'length;
        alias a_value   : t_net_array(1 to length) is value;
        variable result : t_packed_logic(1 to length);
    begin
        for i in 1 to length loop
            result(i) := to_logic(a_value(i));
        end loop;
        return result;
    end function;

    function to_logic (value : t_resolved_net_array) return t_packed_logic is
        constant length : natural := value'length;
        alias a_value   : t_resolved_net_array(1 to length) is value;
        variable result : t_packed_logic(1 to length);
    begin
        for i in 1 to length loop
            result(i) := to_logic(a_value(i));
        end loop;
        return result;
    end function;

    function to_net_value (value : t_logic) return t_net_value is
    begin
        case value is
            when 'X' => return 'X';
            when '0' => return strong0;
            when '1' => return strong1;
            when 'Z' => return highz1;
        end case;
    end function;

    function to_net_value (value : t_packed_logic) return t_net_array is
        constant length : natural := value'length;
        alias a_value   : t_packed_logic(1 to length) is value;
        variable result : t_net_array(1 to length);
    begin
        for i in 1 to length loop
            result(i) := to_net_value(a_value(i));
        end loop;
        return result;
    end function;

    function to_net_value (value : t_packed_logic) return t_resolved_net_array is
        constant length : natural := value'length;
        alias a_value   : t_packed_logic(1 to length) is value;
        variable result : t_resolved_net_array(1 to length);
    begin
        for i in 1 to length loop
            result(i) := to_net_value(a_value(i));
        end loop;
        return result;
    end function;


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
            when 'X' => return 'U';
        end case;
    end function;

    function to_vhdl (value : t_net_value) return std_ulogic is
    begin
        case value is
            when 'X' =>
                return 'U';
            when supply0 | strong0 | pull0 | large0 =>
                return '0';
            when weak0 | medium0 | small0 =>
                return 'L';
            when highz0 | highz1 =>
                return 'Z';
            when small1 | medium1 | weak1 =>
                return 'H';
            when large1 | pull1 | strong1 | supply1 =>
                return '1';
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

    function to_verilog (value : std_ulogic) return t_net_value is
    begin
        case value is
            when '1' => return strong1;
            when 'H' => return weak1;
            when '0' => return strong0;
            when 'L' => return weak0;
            when 'Z' => return highz1;
            when others => return 'X';
        end case;
    end function;

    function to_string (value : t_packed_logic) return string is
        constant length  : natural := value'length;
        constant lookup  : string(1 to 4) := "XZ01";
        variable result  : string(1 to length);
        alias a_value : t_packed_logic(1 to length) is value;
    begin
        for i in 1 to length loop
            result(i) := lookup(t_logic'pos(a_value(i)) + 1);
        end loop;
        return result;
    end function;

    function resize (value : t_packed_logic; length : natural) return t_packed_logic is
        constant orig : natural := value'length;
        alias a_value : t_packed_logic(1 to orig) is value;
    begin
        if length = orig then
            return value;
        elsif length < orig then
            return a_value(1 to length);
        else
            return (1 to length - orig => '0') & value;
        end if;
    end function;

    function resize (value : t_logic; length : natural) return t_packed_logic is
        subtype rtype is t_packed_logic(1 to length);
    begin
        return rtype'(1 => value, others => '0');
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

    function "or" (l, r : t_logic) return t_logic is
    begin
        if l = '0' and r = '0' then
            return '0';
        elsif l = 'X' or r = 'X' or l = 'Z' or r = 'Z' then
            return 'X';
        else
            return '1';
        end if;
    end function;

    function "xor" (l, r : t_logic) return t_logic is
    begin
        if (l = '1' and r = '0') or (l = '0' and r = '1') then
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

    function "not" (x : t_logic) return t_logic is
    begin
        case x is
            when '0' => return '1';
            when '1' => return '0';
            when others => return 'X';
        end case;
    end function;

    function "not" (x : t_packed_logic) return t_packed_logic is
        constant len    : natural := x'length;
        alias xa        : t_packed_logic(0 to len - 1) is x;
        variable result : t_packed_logic(0 to len - 1);
    begin
        for i in result'range loop
            result(i) := not xa(i);
        end loop;
        return result;
    end function;

    function "not" (x : t_packed_logic) return t_logic is
        variable result : t_logic := '1';
    begin
        for i in x'range loop
            case x(i) is
                when '1' => result := '0';
                when '0' => null;
                when others => return 'X';
            end case;
        end loop;
        return result;
    end function;

    function add_unsigned (l, r : t_packed_logic; c : t_logic) return t_packed_logic is
        constant l_left : integer := l'length - 1;
        alias xl        : t_packed_logic(0 to l_left) is l;
        alias xr        : t_packed_logic(0 to l_left) is r;
        variable result : t_packed_logic(0 to l_left);
        variable cbit   : t_logic := c;
    begin
        for i in 0 to l_left loop
            result(i) := cbit xor xl(i) xor xr(i);
            cbit      := (cbit and xl(i)) or (cbit and xr(i)) or (xl(i) and xr(i));
        end loop;
        return result;
    end function;

    function "+" (l, r : t_packed_logic) return t_packed_logic is
        constant size : natural := maximum(l'length, r'length);
        variable lext : t_packed_logic(size - 1 downto 0) := resize(l, size);
        variable rext : t_packed_logic(size - 1 downto 0) := resize(r, size);
    begin
        return add_unsigned(lext, rext, '0');
    end function;

    function "=" (l, r : t_packed_logic) return boolean is
        constant lsize   : natural := l'length;
        constant rsize   : natural := r'length;
        constant minsize : natural := minimum(lsize, rsize);
        alias la : t_packed_logic(1 to lsize) is l;
        alias ra : t_packed_logic(1 to rsize) is r;
    begin
        for i in 1 to minsize loop
            if la(i) /= ra(i) then
                return false;
            end if;
        end loop;
        if lsize > rsize then
            for i in minsize + 1 to lsize loop
                if la(i) /= '0' then
                    return false;
                end if;
            end loop;
        elsif rsize > lsize then
            for i in minsize + 1 to rsize loop
                if ra(i) /= '0' then
                    return false;
                end if;
            end loop;
        end if;
        return true;
    end function;

    function "/=" (l, r : t_packed_logic) return boolean is
    begin
        return not (l = r);
    end function;
end package body;

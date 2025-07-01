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

    procedure encode_net (result : out t_net_value;
                          value : in t_logic;
                          strength0, strength1 : in t_strength) is
    begin
        result := t_logic'pos(value)
            + t_strength'pos(strength0) * 4
            + t_strength'pos(strength1) * 32;
    end procedure;

    procedure decode_net (net : in t_net_value;
                          value : out t_logic;
                          strength0, strength1 : out t_strength) is
    begin
        value := t_logic'val(net rem 4);
        strength0 := t_strength'val((net / 4) rem 8);
        strength1 := t_strength'val((net / 32) rem 8);
    end procedure;

    function resolve_wire (inputs : t_net_array) return t_net_value is
        variable result    : t_net_value;
        variable data      : t_logic := 'Z';
        variable strength0 : t_strength := HiZ;
        variable strength1 : t_strength := HiZ;
        variable elem      : t_logic;
        variable elem_st0  : t_strength;
        variable elem_st1  : t_strength;
        constant count     : natural := inputs'length;
        alias a_inputs     : t_net_array(1 to count) is inputs;
    begin
        if inputs'length > 0 then
            decode_net(a_inputs(1), data, strength0, strength1);
            for i in 2 to count loop
                decode_net(a_inputs(i), elem, elem_st0, elem_st1);
                -- TODO: this is incorrect
                if elem = '1' and elem_st1 > strength1 then
                    data := '1';
                    strength1 := elem_st1;
                    strength0 := elem_st0;
                elsif elem = '0' and elem_st0 > strength0 then
                    data := '0';
                    strength1 := elem_st1;
                    strength0 := elem_st0;
                elsif (elem = '0' and data = '1' and elem_st0 = strength1)
                    or (elem = '1' and data = '0' and elem_st1 = strength0)
                then
                    data := 'X';
                    strength1 := st;
                    strength0 := st;
                end if;
            end loop;
        end if;
        encode_net(result, data, strength0, strength1);
        return result;
    end function;

    function to_logic (value : t_net_value) return t_logic is
        variable result : t_logic;
        variable strength0, strength1 : t_strength;
    begin
        decode_net(value, result, strength0, strength1);
        return result;
    end function;

    function to_logic (value : t_net_array) return t_logic_array is
        constant length : natural := value'length;
        alias a_value   : t_net_array(length - 1 downto 0) is value;
        variable result : t_logic_array(length - 1 downto 0);
    begin
        for i in result'range loop
            result(i) := to_logic(a_value(i));
        end loop;
        return result;
    end function;

    function to_logic (value : t_wire_array) return t_logic_array is
        constant length : natural := value'length;
        alias a_value   : t_wire_array(length - 1 downto 0) is value;
        variable result : t_logic_array(length - 1 downto 0);
        variable strength0, strength1 : t_strength;
    begin
        for i in result'range loop
            decode_net(a_value(i), result(i), strength0, strength1);
        end loop;
        return result;
    end function;

    function to_logic (value : t_int64; width : natural) return t_logic_array is
        variable result : t_logic_array(width - 1 downto 0);
        variable b_val  : t_logic := '0';
        variable i_val  : t_int64 := value;
    begin
        if width < 1 then
            return result;
        elsif value < 0 then
            b_val := '1';
            i_val := -(value + 1);
        end if;
        for i in 0 to result'left loop
            if (i_val mod 2) = 0 then
                result(i) := b_val;
            elsif b_val = '1' then
                result(i) := '0';
            else
                result(i) := '1';
            end if;
            i_val := i_val/2;
        end loop;
        return result;
    end function;

    function to_net (value : t_logic; strength : t_net_value) return t_net_value is
    begin
        assert strength rem 4 = 0;
        return strength + t_logic'pos(value);
    end function;

    function to_net (value : t_logic_array;
                     strength : t_net_value) return t_net_array is
        constant length : natural := value'length;
        alias a_value   : t_logic_array(length - 1 downto 0) is value;
        variable result : t_net_array(length - 1 downto 0);
    begin
        for i in result'range loop
            result(i) := strength + t_logic'pos(a_value(i));
        end loop;
        return result;
    end function;

    function to_net (value : t_logic) return t_net_value is
        variable result : t_net_value;
    begin
        encode_net(result, value, St, St);
        return result;
    end function;

    function to_net (value : t_logic_array) return t_net_array is
        constant length : natural := value'length;
        alias a_value   : t_logic_array(length - 1 downto 0) is value;
        variable result : t_net_array(length - 1 downto 0);
    begin
        for i in result'range loop
            encode_net(result(i), a_value(i), St, St);
        end loop;
        return result;
    end function;

    function to_net (value : t_logic_array) return t_wire_array is
        constant length : natural := value'length;
        alias a_value   : t_logic_array(length - 1 downto 0) is value;
        variable result : t_wire_array(length - 1 downto 0);
    begin
        for i in result'range loop
            encode_net(result(i), a_value(i), St, St);
        end loop;
        return result;
    end function;

    function to_integer (value : t_logic_array) return t_int64 is
        alias v    : t_logic_array(value'length - 1 downto 0) is value;
        variable r : t_int64 := 0;
    begin
        for i in v'range loop
            r := r * 2;
            if v(i) = '1' then
                r := r + 1;
            end if;
        end loop;
        return r;
    end function;

    function to_time (value : t_logic_array) return delay_length is
        alias v    : t_logic_array(value'length - 1 downto 0) is value;
        variable r : delay_length := 0 fs;
    begin
        for i in v'range loop
            r := r * 2;
            if v(i) = '1' then
                r := r + 1 fs;
            end if;
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
        variable data : t_logic;
        variable strength0, strength1 : t_strength;
    begin
        decode_net(value, data, strength0, strength1);
        case data is
            when 'X' =>
                return 'U';
            when '0' =>
                if strength0 = We or strength0 = Me or strength0 = Sm then
                    return 'L';
                else
                    return '0';
                end if;
            when '1' =>
                if strength1 = We or strength1 = Me or strength1 = Sm then
                    return 'H';
                else
                    return '1';
                end if;
            when 'Z' =>
                return 'Z';
        end case;
    end function;

    function to_vhdl (value : t_wire_array) return std_ulogic_vector is
        alias xvalue : t_wire_array(1 to value'length) is value;
        variable result : std_ulogic_vector(1 to value'length);
    begin
        for i in 1 to value'length loop
            result(i) := to_vhdl(xvalue(i));
        end loop;
        return result;
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
        variable result : t_net_value;
        variable data : t_logic;
        variable strength0, strength1 : t_strength := St;
    begin
        case value is
            when '1' =>
                data := '1';
            when 'H' =>
                data := '1';
                strength0 := We;
                strength1 := We;
            when '0' =>
                data := '0';
            when 'L' =>
                data := '0';
                strength0 := We;
                strength1 := We;
            when 'Z' =>
                data := 'Z';
                strength0 := HiZ;
                strength1 := HiZ;
            when others =>
                data := 'X';
        end case;
        encode_net(result, data, strength0, strength1);
        return result;
    end function;

    function to_verilog (value : std_ulogic_vector) return t_net_array is
        alias xvalue : std_ulogic_vector(1 to value'length) is value;
        variable result : t_net_array(1 to value'length);
    begin
        for i in 1 to value'length loop
            result(i) := to_verilog(xvalue(i));
        end loop;
        return result;
    end function;

    function to_string (value : t_logic_array) return string is
        constant length  : natural := value'length;
        constant lookup  : string(1 to 4) := "XZ01";
        variable result  : string(1 to length);
        alias a_value : t_logic_array(length downto 1) is value;
    begin
        for i in 1 to length loop
            result(i) := lookup(t_logic'pos(a_value(i)) + 1);
        end loop;
        return result;
    end function;

    function resize (value : t_logic_array; length : natural) return t_logic_array is
        constant orig : natural := value'length;
        alias a_value : t_logic_array(orig - 1 downto 0) is value;
    begin
        if length = orig then
            return value;
        elsif length < orig then
            return a_value(length - 1 downto 0);
        else
            return (1 to length - orig => '0') & value;
        end if;
    end function;

    function resize (value : t_logic; length : natural) return t_logic_array is
        subtype rtype is t_logic_array(length - 1 downto 0);
    begin
        return rtype'(0 => value, others => '0');
    end function;

end package body;

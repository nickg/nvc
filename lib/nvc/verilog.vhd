-------------------------------------------------------------------------------
--  Copyright (C) 2023-2025  Nick Gasson
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
-- This package provides support routines for Verilog simulation
-------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;

package verilog is

    type t_int64 is range -9223372036854775807 - 1 to 9223372036854775807;

    type t_logic is ('0', '1', 'Z', 'X');

    type t_logic_array is array (natural range <>) of t_logic;

    type t_strength is (HiZ, Sm, Me, We, La, Pu, St, Su);

    -- Bit encoding
    --   7..5 : strength1
    --   4..2 : strength0
    --   1..0 : logic value
    type t_net_value is range 0 to 255;

    type t_net_array is array (natural range <>) of t_net_value;

    function resolve_wire (inputs : t_net_array) return t_net_value;

    subtype t_wire is resolve_wire t_net_value;

    type t_wire_array is array (natural range <>) of t_wire;

    function to_logic (value : t_net_value) return t_logic;
    function to_logic (value : t_net_array) return t_logic_array;
    function to_logic (value : t_wire_array) return t_logic_array;
    function to_logic (value : t_int64; width : natural) return t_logic_array;

    function to_net (value : t_logic; strength : t_net_value) return t_net_value;
    function to_net (value : t_logic_array; strength : t_net_value) return t_net_array;
    function to_net (value : t_logic) return t_net_value;
    function to_net (value : t_logic_array) return t_net_array;
    function to_net (value : t_logic_array) return t_wire_array;

    function to_integer (value : t_logic_array) return t_int64;

    function to_time (value : t_logic_array) return delay_length;

    function to_vhdl (value : t_logic) return std_ulogic;
    function to_vhdl (value : t_net_value) return std_ulogic;

    function to_verilog (value : std_ulogic) return t_logic;
    function to_verilog (value : std_ulogic) return t_net_value;

    function to_string (value : t_logic_array) return string;

    function resize (value : t_logic_array; length : natural) return t_logic_array;
    function resize (value : t_logic; length : natural) return t_logic_array;

end package;

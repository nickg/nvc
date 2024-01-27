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

-------------------------------------------------------------------------------
-- This package provides support routines for Verilog simulation
-------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;

package verilog is

    type t_int64 is range -9223372036854775807 - 1 to 9223372036854775807;

    type t_logic is ('X', 'Z', '0', '1');

    type t_packed_logic is array (natural range <>) of t_logic;

    function to_integer (value : t_packed_logic) return t_int64;

    function to_time (value : t_packed_logic) return delay_length;

    function to_vhdl (value : t_logic) return std_ulogic;

    function to_verilog (value : std_ulogic) return t_logic;

    function resize (value : t_packed_logic; length : natural) return t_packed_logic;
    function resize (value : t_logic; length : natural) return t_packed_logic;

    function "and" (l, r : t_logic) return t_logic;
    function "and" (l, r : t_packed_logic) return t_packed_logic;

    function "not" (x : t_logic) return t_logic;
    function "not" (x : t_packed_logic) return t_packed_logic;
    function "not" (x : t_packed_logic) return t_logic;

end package;

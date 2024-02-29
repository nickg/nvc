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

-------------------------------------------------------------------------------
-- This package provides support routines for Verilog simulation
-------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;

package verilog is

    type t_int64 is range -9223372036854775807 - 1 to 9223372036854775807;

    type t_logic is ('X', 'Z', '0', '1');

    type t_packed_logic is array (natural range <>) of t_logic;

    type t_net_value is ('X', supply0, strong0, pull0, large0, weak0,
                         medium0, small0, highz0, highz1, small1, medium1,
                         weak1, large1, pull1, strong1, supply1);

    type t_net_array is array (natural range <>) of t_net_value;

    function resolved (inputs : t_net_array) return t_net_value;

    subtype t_resolved_net is resolved t_net_value;

    type t_resolved_net_array is array (natural range <>) of t_resolved_net;

    function to_logic (value : t_net_value) return t_logic;
    function to_logic (value : t_net_array) return t_packed_logic;
    function to_logic (value : t_resolved_net_array) return t_packed_logic;

    function to_net_value (value : t_logic) return t_net_value;
    function to_net_value (value : t_packed_logic) return t_net_array;
    function to_net_value (value : t_packed_logic) return t_resolved_net_array;

    function to_integer (value : t_packed_logic) return t_int64;

    function to_time (value : t_packed_logic) return delay_length;

    function to_vhdl (value : t_logic) return std_ulogic;
    function to_vhdl (value : t_net_value) return std_ulogic;

    function to_verilog (value : std_ulogic) return t_logic;
    function to_verilog (value : std_ulogic) return t_net_value;

    function to_string (value : t_packed_logic) return string;

    function resize (value : t_packed_logic; length : natural) return t_packed_logic;
    function resize (value : t_logic; length : natural) return t_packed_logic;

    function "and" (l, r : t_logic) return t_logic;
    function "and" (l, r : t_packed_logic) return t_packed_logic;

    function "xor" (l, r : t_logic) return t_logic;

    function "or" (l, r : t_logic) return t_logic;

    function "not" (x : t_logic) return t_logic;
    function "not" (x : t_packed_logic) return t_packed_logic;
    function "not" (x : t_packed_logic) return t_logic;

    function "+" (l, r : t_packed_logic) return t_packed_logic;

    function "=" (l, r : t_packed_logic) return boolean;

    function "/=" (l, r : t_packed_logic) return boolean;

    procedure sys_finish;

    -- These procedures are called with a special variadic calling convention
    -- which cannot be represented in VHDL
    procedure sys_display (format : string);
    procedure sys_write (format : string);

    attribute foreign of sys_finish : procedure is "INTERNAL __nvc_sys_finish";
    attribute foreign of sys_write : procedure is "INTERNAL __nvc_sys_write";
    attribute foreign of sys_display : procedure is "INTERNAL __nvc_sys_display";

end package;

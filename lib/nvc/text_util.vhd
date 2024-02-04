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
-- Utilities for handling text
-------------------------------------------------------------------------------

package text_util is
    type str_ptr_t is access string;

    type text_buf_t is record
        len   : natural;
        alloc : natural;
        buf   : str_ptr_t;
    end record;

    type t_int64 is range -9223372036854775807 - 1 to 9223372036854775807;

    procedure tb_cat (tb : out text_buf_t; str : in string);

    -- Used to implement 'VALUE and 'IMAGE for scalar types
    function canon_value (s : string) return string;
    function string_to_int (s : string) return t_int64;
    procedure string_to_int (s : in string; value : out t_int64;
                             used : out natural);
    function string_to_real (s : string) return real;
    function int_to_string (x : t_int64) return string;
    function real_to_string (x : real) return string;

    -- Used to implement 'VALUE for composite types
    function next_delimiter (s : string; pos : natural) return string;
    function count_delimiters (s : string) return natural;
    procedure trim_ws (s : in string; first, last : out natural);
    function find_open (s : string) return natural;
    procedure find_close (s : string; pos : natural);
    procedure report_bad_char (s : string; c : character);

    attribute foreign of real_to_string [real return string]
        : function is "INTERNAL _std_to_string_real";
end package;

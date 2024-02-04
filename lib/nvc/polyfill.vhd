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

-------------------------------------------------------------------------------
-- This package provides VHDL-1993 compatibility wrappers for future standard
-- revisions.
-------------------------------------------------------------------------------

package polyfill is

    function to_string (value : real; spec : string) return string;

    function to_string (value : integer) return string;

    function to_hstring (value : bit_vector) return string;

    function to_ostring (value : bit_vector) return string;

    function maximum (x, y : integer) return integer;

    function minimum (x, y : integer) return integer;

    attribute foreign of to_hstring [bit_vector return string] : function
        is "INTERNAL _std_to_hstring_bit_vec";
    attribute foreign of to_ostring [bit_vector return string] : function
        is "INTERNAL _std_to_ostring_bit_vec";
    attribute foreign of to_string [real, string return string] : function
        is "INTERNAL _std_to_string_real_format";

end package;

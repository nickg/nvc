-------------------------------------------------------------------------------
--  Copyright (C) 2021  Nick Gasson
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
-- This package provides implementation of following PSL functions:
--  isunknown
--  countones
--  onehot
--  onehot0
-------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;

package psl_support is

    ---------------------------------------------------------------------------
    -- Returns true if there is at least one of input vector bits is:
    --  'U', 'W', 'X', 'Z', '-'
    ---------------------------------------------------------------------------
    function isunknown(
        val : in std_logic_vector
    ) return boolean;

    function isunknown(
        val : in std_ulogic_vector
    ) return boolean;

    ---------------------------------------------------------------------------
    --  Returns number of '1' or 'H' in the input vector.
    ---------------------------------------------------------------------------
    function countones(
        val : in std_logic_vector
    ) return boolean;

    function countones(
        val : in std_ulogic_vector
    ) return integer;

    function countones(
        val : in bit_vector
    ) return integer;

    ---------------------------------------------------------------------------
    -- Returns true if there is exactly one occurence of '1' or 'H' in the
    -- input vector.
    ---------------------------------------------------------------------------
    function onehot(
        val : in std_logic_vector
    ) return boolean;

    function onehot(
        val : in std_ulogic_vector
    ) return boolean;

    function onehot(
        val : in bit_vector
    ) return boolean;

    ---------------------------------------------------------------------------
    -- Returns true if there is zero or one occurence of '1' or 'H' in the
    -- input vector.
    ---------------------------------------------------------------------------
    function onehot0(
        val : in std_logic_vector
    ) return boolean;

    function onehot0(
        val : in std_ulogic_vector
    ) return boolean;

    function onehot0(
        val : in bit_vector
    ) return boolean;

end package;

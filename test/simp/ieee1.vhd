library ieee;
use ieee.std_logic_1164.all;

package ieee1 is
    constant b1 : std_logic := '1' ?= '1';  -- '1'
    constant b2 : std_logic := '1' ?= '0';  -- '0'
    constant b3 : std_logic := 'H' ?= '1';  -- '1'
end package;

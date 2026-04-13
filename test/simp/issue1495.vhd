library ieee;
use ieee.std_logic_1164.all;

package issue1495 is
    constant c1 : std_logic := '0' ?>= '1'; -- '0'
    constant c2 : std_logic := 'H' ?>= 'L'; -- '1'
    constant c3 : std_logic := '1' ?<= '0'; -- '0'
    constant c4 : std_logic := 'L' ?<= 'H'; -- '1'
    constant c5 : std_logic := "00000010" ?= "00000010"; -- 1

end package;

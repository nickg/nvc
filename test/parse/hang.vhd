entity ent is begin
end entity ent;

architecture arch of ent is
    constant i : integer := 0e100000000000; -- used to hang here
begin
end architecture arch;

--------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;

package hang is
    constant c0 : std_logic_vector (31 downto 0) := 32sb"1"; -- ok
    -- used to hang on line below
    constant c1 : std_logic_vector (31 downto 0) := 32222222222sb"1";
end package;

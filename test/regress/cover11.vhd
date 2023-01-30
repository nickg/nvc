
library ieee;
use ieee.std_logic_1164.all;

library ieee;
use ieee.std_logic_1164.all;

entity cover11 is
end entity;

architecture test of cover11 is

    -- To test the report limit
    --  Covers realloc code of misses
    signal EXAMPLE_ARRAY : std_logic_vector(0 to 4000);

    -- To cover realloc code for hits and excludes!
    signal ARR_A         : std_logic_vector(1500 downto 0);
    signal ARR_B         : std_logic_vector(1500 downto 0);

begin

    test_ctrl_proc: process
    begin
        wait for 1 ns;
        EXAMPLE_ARRAY(0) <= '1';
        wait for 1 ns;
        EXAMPLE_ARRAY(0) <= '0';
        wait for 1 ns;

        ARR_A <= (others => '0');
        wait for 1 ns;
        ARR_A <= (others => '1');
        wait for 1 ns;

        wait;
    end process;


end architecture;

entity wave1 is
end entity;

library ieee;
use ieee.std_logic_1164.all;

architecture test of wave1 is
    signal x : std_logic;
    signal y : std_logic_vector(1 to 3) := "ZZZ";
begin

    main: process is
    begin
        x <= '1';
        wait for 1 ns;
        y <= "101";
        x <= '0';
        wait for 1 ns;
        y <= "001";
        wait;
    end process;

end architecture;

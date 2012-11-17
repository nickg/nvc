entity driver2 is
end entity;

library ieee;
use ieee.std_logic_1164.all;

architecture test of driver2 is
    signal x : std_logic;
begin

    x <= 'H';

    p1: process is
    begin
        x <= 'Z';
        wait for 1 ns;
        assert x = 'H';
        x <= '0';
        wait for 1 ns;
        assert x = '0';
        x <= 'Z';
        wait for 1 ns;
        assert x = 'H';
        wait;
    end process;

end architecture;

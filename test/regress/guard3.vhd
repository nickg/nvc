entity guard3 is
end entity;

library ieee;
use ieee.std_logic_1164.all;

architecture test of guard3 is
    signal s : std_logic bus := 'H';
begin

    p1: process is
    begin
        assert s = 'H';
        s <= '0';
        wait for 1 ns;
        assert s = '0';
        s <= null after 5 ns;
        wait for 1 ns;
        assert s = '0';
        wait for 5 ns;
        assert s = 'Z';
        s <= '1' after 1 ns;
        wait for 0 ns;
        assert s = 'Z';
        wait for 1 ns;
        assert s = '1';
        wait;
    end process;

end architecture;

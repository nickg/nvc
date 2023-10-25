entity mixed1 is
end entity;

library ieee;
use ieee.std_logic_1164.all;

architecture test of mixed1 is
    component clkbuf is
        port ( i : in std_logic;
               o : out std_logic );
    end component;

    signal i, o : std_logic;
begin

    u: component clkbuf
        port map (i, o);

    test: process is
    begin
        assert o = 'U';
        i <= '1';
        wait for 1 ns;
        assert o = '1';
        i <= 'H';
        wait for 1 ns;
        assert o = '1';
        i <= 'L';
        wait for 1 ns;
        assert o = '0';
        i <= 'Z';
        wait for 1 ns;
        assert o = 'Z';
        i <= '-';
        wait for 1 ns;
        assert o = 'U';
        wait;
    end process;

end architecture;

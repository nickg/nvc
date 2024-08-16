entity mixed2 is
end entity;

library ieee;
use ieee.std_logic_1164.all;

architecture test of mixed2 is
    component multiplexer is
        port ( mux : out std_logic;
               control, dataA, dataB : in std_logic );
    end component;

    signal mux, control, dataA, dataB : std_logic;
begin

    uut: component multiplexer
        port map ( mux, control, dataA, dataB);

    check: process is
    begin
        wait for 0 ns;
        assert mux = 'U';

        dataA <= '1';
        dataB <= '0';
        control <= '0';
        wait for 1 ns;
        assert mux = '1';

        control <= '1';
        wait for 1 ns;
        assert mux = '0';

        dataB <= '1';
        wait for 1 ns;
        assert mux = '1';

        wait;
    end process;

end architecture;

entity timescale1 is
end entity;

library ieee;
use ieee.std_logic_1164.all;

architecture test of timescale1 is
    component sub is
        port ( x : out std_logic_vector(7 downto 0) );
    end component;

    signal x : std_logic_vector(7 downto 0);
begin

    u: component sub port map (x);

    postponed process is
    begin
        wait for 1 ns;
        assert x = X"01";
        wait for 2 ns;
        assert x = X"01";
        wait for 1 ns;
        assert x = X"02";
        wait for 999 us;
        assert x = X"02";
        wait for 1 us;
        assert x = X"03";
        wait;
    end process;

end architecture;

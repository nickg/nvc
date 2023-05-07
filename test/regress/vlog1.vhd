entity vlog1 is
end entity;

library ieee;
use ieee.std_logic_1164.all;

architecture test of vlog1 is

    component dff is
        port (
            d, clk, rstb : in std_logic;
            q : out std_logic );
    end component;

    signal d, clk, rstb, q : std_logic;
begin

    uut: component dff
        port map ( d, clk, rstb, q );

    main: process is
    begin
        rstb <= '0';
        clk <= '0';
        d <= '0';
        wait for 1 ns;
        rstb <= '1';
        wait for 1 ns;
        d <= '1';
        wait for 1 ns;
        clk <= '1';
        wait for 1 ns;
        clk <= '0';
        d <= '0';
        wait for 1 ns;
        assert q = '1';
        wait;
    end process;

end architecture;

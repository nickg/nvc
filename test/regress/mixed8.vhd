entity mixed8 is
end entity;

library ieee;
use ieee.std_logic_1164.all;

architecture test of mixed8 is
    component top is
        port ( clk : in std_logic; pass : out std_logic );
    end component;

    signal clk : std_logic := '0';
    signal pass : std_logic;
begin

    uut: component top port map ( clk, pass );

    process is
    begin
        clk <= not clk after 5 ns;
        wait for 10 ns;
        assert pass = '1';
        wait;
    end process;

end architecture;

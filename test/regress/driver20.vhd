library ieee;
use ieee.std_logic_1164.all;

entity driver20 is
end entity;

architecture test of driver20 is
    constant PERIOD : time := 10 ns;
    signal clk      : std_logic;
    signal running  : boolean := true;
begin

    g: for i in 1 to 2 generate
        b: block is
            port ( oclk : out std_logic );
            port map ( clk );

            signal xclk : std_logic := '1';
        begin
            xclk <= not xclk after PERIOD/2 when running;
            oclk <= xclk;
        end block;
    end generate;

    check: process is
    begin
        assert clk = 'U';

        wait for 0 ns;

        assert clk = '1';
        assert not rising_edge(clk);
        assert not falling_edge(clk);

        wait for PERIOD / 2;

        assert clk = '1';
        assert not rising_edge(clk);
        assert not falling_edge(clk);

        wait for 0 ns;

        assert clk = '0';
        assert not rising_edge(clk);
        assert falling_edge(clk);

        wait for PERIOD / 2;

        assert clk = '0';
        assert not rising_edge(clk);
        assert not falling_edge(clk);

        wait for 0 ns;

        assert clk = '1';
        assert rising_edge(clk);
        assert not falling_edge(clk);

        running <= false;
        wait;
    end process;

end architecture;

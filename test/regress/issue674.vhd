entity top is
    generic (clk_period : time);
end top;

architecture str of top is
    signal rst : bit := '1';
begin

    RELEASE_RESET : process
    begin
        wait for clk_period * 10;
        rst <= '0';
        wait for clk_period * 1;
        wait;
    end process;

end architecture;

use std.env.finish;

entity issue674 is
end issue674;

architecture sim of issue674 is
    constant clk_hz : integer := 100e6;
    constant clk_period : time := 1 sec / clk_hz;
begin

    DUT : entity work.top(str)
        generic map(clk_period => clk_period);

    SEQUENCER_PROC : process
    begin
        wait until << signal dut.rst : bit >>= '0';
        assert now = 100 ns;
        finish;
    end process;

end architecture;

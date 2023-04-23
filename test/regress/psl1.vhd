entity psl1 is
end entity;

library ieee;
use ieee.std_logic_1164.all;

architecture test of psl1 is
    signal clk : std_logic := '0';
    signal x, y : std_logic := '0';
    signal running : boolean := true;

    -- psl default clock is rising_edge(clk);
begin

    -- psl a1: assert always x or y;
    -- psl a2: assert always x -> next y;

    clkgen: clk <= not clk after 5 ns when running;

    stim: process is
    begin
        wait until falling_edge(clk);
        x <= '1';
        wait until falling_edge(clk);
        x <= '0';
        y <= '1';
        wait until falling_edge(clk);
        x <= '1';
        y <= '0';
        wait until falling_edge(clk);

        running <= false;
        wait;
    end process;

end architecture;

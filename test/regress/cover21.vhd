entity cover21 is
end entity;

library ieee;
use ieee.std_logic_1164.all;

architecture test of cover21 is
    signal clk : std_logic := '0';
    signal x, y : std_logic;

    -- psl default clock is rising_edge(clk);
begin

    -- psl one: cover {x and y};
    -- psl two: cover {[*]; x and y};
    -- psl three: cover {x; not x and not y; y};
    -- psl four: cover {x; not x; x and y};

    clkgen: clk <= not clk after 5 ns when now < 50 ns;

    seq: process is
    begin
        x <= '1';
        wait for 1 ns;
        y <= '1';
        wait for 6 ns;
        y <= '0';
        wait for 10 ns;
        x <= '0';
        wait for 10 ns;
        y <= '1';
        wait for 10 ns;
        wait;
    end process;

end architecture;

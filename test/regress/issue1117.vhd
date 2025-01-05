entity issue1117 is
end entity;

architecture test of issue1117 is
    signal clk, x, y : bit;
    signal running : boolean := true;

    -- psl default clock is rising_edge(clk);
begin
    -- psl assert never x and y report "x and y both 1";

    clkgen: clk <= not clk after 10 ns when running;

    check: process is
    begin
        wait until rising_edge(clk);
        x <= '1';
        y <= '1';
        wait for 1 ns;                  -- No failure here
        x <= '0';
        wait until rising_edge(clk);

        running <= false;
        wait;
    end process;

end architecture;

entity issue1693 is
end entity;

architecture test of issue1693 is
    signal clk : bit := '0';
    signal a   : bit := '0';
begin
    clk <= not clk after 5 ns when now < 100 ns;

    -- psl direct: assert (always a = '0') @rising_edge(clk);

    g_if: if true generate
        -- psl nested: assert (always a = '0') @rising_edge(clk);
    end generate;

    g_for: for i in 1 to 2 generate
        -- psl repeated: assert (always a = '0') @rising_edge(clk);
    end generate;

    stim: process is
    begin
        for i in 1 to 8 loop
            wait until falling_edge(clk);
            a <= '1';
            wait for 1 ns;
            a <= '0';
        end loop;
        wait;
    end process;

    process is
    begin
        wait for 100 ns;
        report "PASSED";
        wait;
    end process;
end architecture;

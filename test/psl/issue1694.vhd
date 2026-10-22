entity issue1694 is
end entity;

architecture test of issue1694 is
    signal clk : bit;
    signal a   : bit;

    -- psl default clock is rising_edge(clk);
begin
    g1: if true generate
        -- psl default clock is rising_edge(clk);
    begin
        -- psl assert always a = '0';
    end generate;

    g2: for i in 1 to 1 generate
        default clock is rising_edge(clk);
    begin
        assert always a = '0';
    end generate;

    g3: if true generate
        -- psl assert always a = '0';
    end generate;
end architecture;

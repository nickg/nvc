entity issue904 is
end entity;

architecture test of issue904 is
    signal x, y, clk : bit;

    -- psl default clock is clk'event and clk = '1';
begin

    clk <= not clk after 1 ns when now < 10 ns;

    x <= '1' after 1 ns, '0' after 3 ns, '1' after 5 ns;
    y <= '1' after 2 ns;

    -- psl cover {x = '1' and y = '1'};

end architecture;

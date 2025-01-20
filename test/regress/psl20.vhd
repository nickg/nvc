entity psl20 is
end entity;

architecture tb of psl20 is

    signal clk : natural;
    signal a : bit;

    constant seq_a : bit_vector := "00111010";

begin

    clkgen: clk <= clk + 1 after 1 ns when clk < 7;

    agen: a <= seq_a(clk);

    -- psl default clock is clk'delayed(0 ns)'event;

    -- Covered at 2 ns and 6 ns
    -- psl one: cover {rose(a)} report "ROSE";

    -- Covered at 5 ns and 7 ns
    -- psl two: cover {fell(a)} report "FELL";

    -- Covered at 3 ns and 4 ns
    -- TODO: should be covered at 1 ns too! Why isn't it ?
    -- psl three: cover {stable(a)} report "STABLE";

end architecture;

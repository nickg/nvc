entity psl17 is
end entity;

architecture tb of psl17 is

    signal clk : natural;
    signal a,b,c,d : bit;

    constant seq_a : bit_vector := "0100" & "1000";
    constant seq_b : bit_vector := "0110" & "0100";
    constant seq_c : bit_vector := "0001" & "0100";
    constant seq_d : bit_vector := "0001" & "0000";

begin

    clkgen: clk <= clk + 1 after 1 ns when clk < 7;

    agen: a <= seq_a(clk);
    bgen: b <= seq_b(clk);
    cgen: c <= seq_c(clk);
    dgen: d <= seq_d(clk);

    -- psl default clock is clk'delayed(0 ns)'event;

    -- Covered at 3ns
    -- psl one: cover {{a;b;c} && {b;b;c}} report "one";

    -- Covered at 5ns
    -- psl two: cover {{b[=3]} && {c[=1]; (not c)[+]; c}} report "two";

end architecture;

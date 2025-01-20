entity psl19 is
end entity;

architecture tb of psl19 is

    signal clk : natural;
    signal a,b,c : bit;
    signal v : bit_vector(15 downto 0);

    constant seq_a : bit_vector := "01110000";
    constant seq_b : bit_vector := "01101000";
    constant seq_c : bit_vector := "01000010";

    type t_matrix is array (0 to 7) of bit_vector(15 downto 0);
    constant seq_v : t_matrix :=
        (
            (x"0000"),
            (x"0000"),
            (x"0000"),
            (x"EEEE"),
            (x"DDDD"),
            (x"CCCC"),
            (x"BBBB"),
            (x"AAAA")
        );

begin

    clkgen: clk <= clk + 1 after 1 ns when clk < 7;

    agen: a <= seq_a(clk);
    bgen: b <= seq_b(clk);
    cgen: c <= seq_c(clk);

    vgen: v <= seq_v(clk);

    -- psl default clock is clk'delayed(0 ns)'event;

    -- Covered at 2 ns and 3 ns
    -- psl one: cover {a and prev(a)} report "one";

    -- Covered at 4 ns
    -- psl two: cover {b and prev(b,2)} report "two";

    -- Covered at 6 ns
    -- psl three: cover {c and prev(c,5)} report "three";

    -- Covered at 7 ns
    -- psl four: cover {     v    = x"AAAA" and
    --                  prev(v  ) = x"BBBB" and
    --                  prev(v,2) = x"CCCC" and
    --                  prev(v,3) = x"DDDD" and
    --                  prev(v,4) = x"EEEE"} report "four";

end architecture;

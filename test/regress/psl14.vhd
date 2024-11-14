entity psl14 is
end entity;

architecture tb of psl14 is

    signal clk : natural;
    signal a,b,c,d,e : bit;

    constant seq_a : bit_vector := "1010101000000000";
    constant seq_b : bit_vector := "0101010100000000";
    constant seq_c : bit_vector := "0000000011100000";
    constant seq_d : bit_vector := "0000011111110000";
    constant seq_e : bit_vector := "0000000011111110";

begin

    clkgen: clk <= clk + 1 after 1 ns when clk < 15;

    agen: a <= seq_a(clk);
    bgen: b <= seq_b(clk);
    cgen: c <= seq_c(clk);
    dgen: d <= seq_d(clk);
    egen: e <= seq_e(clk);

    -- psl default clock is clk'event;

    -- Should be hit at:
    --  6 ns - 3 repetitions (started at 0 ns)
    --  8 ns - 4 repetitions (started at 0 ns)
    --  8 ns - 3 repetitions (started at 2 ns)
    -- psl cov_1 : cover {a;b}[*3 to 4] report "cov_1 hit";

    -- Should be hit at:
    --  10 ns - First two ones from "111" in seq_c
    --  11 ns - Last two ones from "111" in seq_c
    -- psl cov_2 : cover {c;c}[+] report "cov_2 hit";

    -- Should be hit at:
    --  12 ns - Sequence of "1111111" from seq_d
    -- psl cov_3 : cover {d}[*7] report "cov_3 hit";

    -- Should be hit at:
    --  13 ns - After 5 repetitions
    --  14 ns - After 6 repetitions
    --  15 ns - After 7 repetitions
    -- psl cov_5 : cover {e}[*5 to inf] report "cov_5 hit";

end architecture;

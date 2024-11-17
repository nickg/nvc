entity psl15 is
end entity;

architecture tb of psl15 is

    signal clk : natural;
    signal a,b,c,d,e,f,g,h,i,j,k,l : bit;

    --    Parts                       A        B            C
    constant seq_a : bit_vector := "1000" & "10000" & "1000000000";
    constant seq_b : bit_vector := "0110" & "01010" & "0010100100";
    constant seq_c : bit_vector := "0001" & "00001" & "0000001110";

    --    Parts                       A          B           C
    constant seq_d : bit_vector := "1000" & "100000000" & "100000";
    constant seq_e : bit_vector := "0110" & "001000100" & "011100";
    constant seq_f : bit_vector := "0011" & "000100001" & "001010";

    --    Parts                         A             B
    constant seq_g : bit_vector := "100000000" & "1000000000";
    constant seq_h : bit_vector := "001010100" & "0111000000";
    constant seq_i : bit_vector := "000000010" & "0000010000";

    --    Parts                         A
    constant seq_j : bit_vector := "1000000000000000000";
    constant seq_k : bit_vector := "0101001001100100100";
    constant seq_l : bit_vector := "0010100100010010000";

begin

    clkgen: clk <= clk + 1 after 1 ns when clk < 18;

    agen: a <= seq_a(clk);
    bgen: b <= seq_b(clk);
    cgen: c <= seq_c(clk);
    dgen: d <= seq_d(clk);
    egen: e <= seq_e(clk);
    fgen: f <= seq_f(clk);
    ggen: g <= seq_g(clk);
    hgen: h <= seq_h(clk);
    igen: i <= seq_i(clk);
    jgen: j <= seq_j(clk);
    kgen: k <= seq_k(clk);
    lgen: l <= seq_l(clk);

    -- psl default clock is clk'event;

    -- Should be covered at:
    --  4 ns - Part A - Consecutive repeat
    --  9 ns - Part B - Empty cycle in the middle
    -- 16 ns - Part C - Empty cycle in the start and back
    -- 17 ns - Part C - Same as previous, continuation of c
    -- Should not be covered at 18 ns, since b occured again at 17 ns.
    -- psl cov_1 : cover {a;b[=2];c} report "cov_1 hit";

    -- Should be covered at:
    --  3 ns - Part A - Due to 1 repetition of e and f in next cycle
    --  4 ns - Part A - Due to 2 repetitions of e and f in next cycle
    --  8 ns - Part B - Due to 1 repetition of e and f in next cycle
    -- 13 ns - Part B - Due to 2 repetitions of e and f in next cycle
    -- 16 ns - Part C - Due to 1 repetition of e and f in next cycle
    -- Should not be covered later since e was repeated 3 times before
    -- another f
    -- psl cov_2 : cover {d;e[=1 to 2];f} report "cov_2 hit";

    -- Should be covered at:
    --  8 ns - Part A - 3 repetitions of h followed by i in the next cycle
    --  Should not be covered in part B since there is one cycle gap
    --  after last h before i is high
    -- psl cov_3 : cover {g;h[->3];i} report "cov_3 hit";

    -- Should be covered at:
    --  5 ns - After 2 repetitions of k and l in next cycle
    --  8 ns - After 3 repetitions of k and l in next cycle
    -- 12 ns - After 5 repetitions of k and l in next cycle
    -- 15 ns - After 6 repetitions of k and l in next cycle
    -- Should not be covered after 4 and 7 repetitions of k since l is
    -- not active in the next cycle
    -- psl cov_4 : cover {j;k[->2 to inf];l} report "cov_4 hit";

end architecture;

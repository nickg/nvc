entity psl16 is
end entity;

architecture tb of psl16 is

    signal clk : natural;
    signal a,b,c,d,e,f : bit;

    --    Parts                       A        B            C
    constant seq_a : bit_vector := "011" & "10000" & "10000";
    constant seq_b : bit_vector := "011" & "01000" & "01000";
    constant seq_c : bit_vector := "110" & "00100" & "01000";

    constant seq_d : bit_vector := "100" & "00100" & "00100";
    constant seq_e : bit_vector := "010" & "00010" & "00100";
    constant seq_f : bit_vector := "001" & "00001" & "00010";

begin

    clkgen: clk <= clk + 1 after 1 ns when clk < 12;

    agen: a <= seq_a(clk);
    bgen: b <= seq_b(clk);
    cgen: c <= seq_c(clk);
    dgen: d <= seq_d(clk);
    egen: e <= seq_e(clk);
    fgen: f <= seq_f(clk);

    -- psl default clock is clk'event;

    -- Should be covered at 2ns since a,b,c are high at the same time.
    -- Should not be covered at 1ns or 3 ns.
    -- psl cov_1 : cover {a:b:c} report "cov_1 hit";

    -- Should be covered at 8 ns.
    -- psl cov_2 : cover {{a;b;c}:{d;e;f}} report "cov_2_hit";

    -- Should be covered at 12 ns.
    -- psl cov_3 : cover {a;b:c;d:e;f} report "cov_3_hit";

end architecture;

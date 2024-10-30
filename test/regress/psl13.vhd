entity psl13 is
end entity;

architecture tb of psl13 is

    signal clk : natural;
    signal a,b,c,d : bit;

    constant seq_a : bit_vector := "0100" & "1000";
    constant seq_b : bit_vector := "0010" & "0100";
    constant seq_c : bit_vector := "0000" & "0010";
    constant seq_d : bit_vector := "0001" & "0000";

begin

    clkgen: clk <= clk + 1 after 1 ns when clk < 7;

    agen: a <= seq_a(clk);
    bgen: b <= seq_b(clk);
    cgen: c <= seq_c(clk);
    dgen: d <= seq_d(clk);

    -- psl default clock is clk'event;

    -- Should fail at: 3 ns and 7 ns
    -- psl asrt_1 : assert always {a;b} |=> {c;d};

    -- Should fail at 2 ns and 5 ns
    -- psl asrt_2 : assert always {a;b} |-> c;

end architecture;

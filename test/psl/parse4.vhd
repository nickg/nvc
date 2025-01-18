entity parse4 is
end entity;

architecture test of parse4 is

    signal a, b, c, clk : bit;

begin

    -- psl default clock is clk'event and clk = '1';

    -- psl sequence seq_a is {a;b;c};
    -- psl sequence seq_b(const rpt) is a[*rpt];

    -- Sequence_Instance
    -- psl cover seq_a;

    -- Sequence_Instance(Actual_Parameter_List)
    -- psl cover seq_b(5);
    -- psl cover seq_b(4, {a;b;c});

    -- Boolean [* [ Count ] ]
    -- psl cover a[*10];
    -- psl cover a='1'[*1][*2][*3];

    -- Sequence [* [ Count ] ]
    -- psl cover seq_b(4)[*2];
    -- psl cover seq_b(4)[*2][*3];

    -- [* [ Count ] ]
    -- psl cover [*5];
    -- psl cover [*5 to 10];

    -- [+]
    -- psl cover [+];

    -- Boolean [= Count ]
    -- psl cover c[=10];
    -- psl cover c[=5 to 6];
    -- psl cover c[=10][*3];

    -- Boolean [-> [ positive_Count ] ]
    -- psl cover b[->4];

    -- Boolean Proc_Block
    -- psl cover a[[signal sig : bit; sig <= '1';]];

    -- Sequence Proc_Block
    -- psl cover {a;b} [[signal sig : bit_vector(3 downto 0); sig <= "0000";]];

    -- psl foo: cover {a;b;c} @ (clk'event and clk = '1');

    -- psl property p1 is {a; a};
    -- psl cover p1(a);            -- Error

    -- Paramterized SERE
    -- psl cover {for i in {1 to 3}: && {seq_b(i)}};

    -- Garbage after PSL directive
    -- psl asfasfa;

end architecture;

Library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity parse4 is
end entity;

architecture test of parse4 is

    signal a, b, c, clk : bit;

begin

    -- psl default clock is rising_edge(clk);

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
    -- psl cover a[[signal sig : std_logic; sig <= '1';]];

    -- Sequence Proc_Block
    -- psl cover {a;b} [[signal sig : unsigned(3 downto 0); sig <= "0000";]];

    -- psl cover {a;b;c} @ rising_edge(clk);

end architecture;

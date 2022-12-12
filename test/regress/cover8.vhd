
library ieee;
use ieee.numeric_std.all;

entity cover8 is
end entity;

architecture test of cover8 is

    -- OR / NOR
    signal lhs_or_1   : boolean := false;
    signal rhs_or_1   : boolean := false;
    signal res_or_1   : boolean;
    signal res_nor_1  : boolean;

    signal lhs_or_2   : boolean := false;
    signal rhs_or_2   : boolean := false;
    signal res_or_2   : boolean;
    signal res_nor_2  : boolean;

    signal res_or_3   : boolean;
    signal res_nor_3  : boolean;

    -- AND / NAND
    signal lhs_and_1  : boolean := false;
    signal rhs_and_1  : boolean := false;
    signal res_and_1  : boolean;
    signal res_nand_1 : boolean;

    signal lhs_and_2  : boolean := true;
    signal rhs_and_2  : boolean := true;
    signal res_and_2  : boolean;
    signal res_nand_2 : boolean;

    signal res_and_3  : boolean;
    signal res_nand_3 : boolean;

    -- XOR / XNOR
    signal lhs_xor_1  : boolean := false;
    signal rhs_xor_1  : boolean := false;
    signal res_xor_1  : boolean;
    signal res_xnor_1 : boolean;

    signal lhs_xor_2  : boolean := false;
    signal rhs_xor_2  : boolean := false;
    signal res_xor_2  : boolean;
    signal res_xnor_2 : boolean;

    signal res_xor_3  : boolean;
    signal res_xnor_3 : boolean;

    -- EQ / NEQ
    signal lhs_eq_1   : boolean := false;
    signal rhs_eq_1   : boolean := false;
    signal res_eq_1   : boolean;
    signal res_neq_1  : boolean;

    signal lhs_eq_2   : boolean := false;
    signal rhs_eq_2   : boolean := true;
    signal res_eq_2   : boolean;
    signal res_neq_2  : boolean;

    signal lhs_eq_3   : boolean := true;
    signal rhs_eq_3   : boolean := true;
    signal res_eq_3   : boolean;
    signal res_neq_3  : boolean;

    -- LT / GT
    signal lhs_lt_1   : integer := 0;
    signal rhs_lt_1   : integer := 0;
    signal res_lt_1   : boolean;
    signal res_gt_1   : boolean;

    signal lhs_lt_2   : integer := 10;
    signal rhs_lt_2   : integer := 5;
    signal res_lt_2   : boolean;
    signal res_gt_2   : boolean;

    signal lhs_lt_3   : integer := 7;
    signal rhs_lt_3   : integer := 20;
    signal res_lt_3   : boolean;
    signal res_gt_3   : boolean;

    signal lhs_lt_4   : integer := 0;
    signal rhs_lt_4   : integer := 0;
    signal res_lt_4   : boolean;
    signal res_gt_4   : boolean;

    -- LEQ / GEQ
    signal lhs_leq_1  : integer := 0;
    signal rhs_leq_1  : integer := 0;
    signal res_leq_1  : boolean;
    signal res_geq_1  : boolean;

    signal lhs_leq_2  : integer := 1;
    signal rhs_leq_2  : integer := 0;
    signal res_leq_2  : boolean;
    signal res_geq_2  : boolean;

    signal lhs_leq_3  : integer := 5;
    signal rhs_leq_3  : integer := 20;
    signal res_leq_3  : boolean;
    signal res_geq_3  : boolean;

    signal lhs_leq_4  : integer := 5;
    signal rhs_leq_4  : integer := 20;
    signal res_leq_4  : boolean;
    signal res_geq_4  : boolean;

begin

    ---------------------------------------------------------------------------
    -- Logic ops on boolean
    ---------------------------------------------------------------------------

    -- OR / NOR
    gate_or_1:
        res_or_1 <= lhs_or_1 or rhs_or_1;
    gate_nor_1:
        res_nor_1 <= lhs_or_1 nor rhs_or_1;

    gate_or_2:
        res_or_2 <= lhs_or_2 or rhs_or_2;
    gate_nor_2:
        res_nor_2 <= lhs_or_2 nor rhs_or_2;

    gate_or_3:
        res_or_3 <= lhs_or_1 or rhs_or_2;
    gate_nor_3:
        res_nor_3 <= lhs_or_1 nor rhs_or_2;

    -- AND / NAND
    gate_and_1:
        res_and_1 <= lhs_and_1 and rhs_and_1;
    gate_nand_1:
        res_nand_1 <= lhs_and_1 nand rhs_and_1;

    gate_and_2:
        res_and_2 <= lhs_and_2 and rhs_and_2;
    gate_nand_2:
        res_nand_2 <= lhs_and_2 nand rhs_and_2;

    gate_and_3:
        res_and_3 <= lhs_and_1 and rhs_and_2;
    gate_nand_3:
        res_nand_3 <= lhs_and_1 nand rhs_and_2;

    -- XOR / XNOR
    gate_xor_1:
        res_xor_1 <= lhs_xor_1 xor rhs_xor_1;
    gate_xnor_1:
        res_xnor_1 <= lhs_xor_1 xnor rhs_xor_1;

    gate_xor_2:
        res_xor_2 <= lhs_xor_2 xor rhs_xor_2;
    gate_xnor_2:
        res_xnor_2 <= lhs_xor_2 xnor rhs_xor_2;

    gate_xor_3:
        res_xor_3 <= lhs_xor_1 xor rhs_xor_2;
    gate_xnor_3:
        res_xnor_3 <= lhs_xor_1 xnor rhs_xor_2;

    -- EQ / NEQ
    gate_eq_1:
        res_eq_1 <= true when (lhs_eq_1 = rhs_eq_1) else false;
    gate_neq_1:
        res_neq_1 <= true when (lhs_eq_1 /= rhs_eq_1) else false;

    gate_eq_2:
        res_eq_2 <= true when (lhs_eq_2 = rhs_eq_2) else false;
    gate_neq_2:
        res_neq_2 <= true when (lhs_eq_2 /= rhs_eq_2) else false;

    gate_eq_3:
        res_eq_3 <= true when (lhs_eq_3 = rhs_eq_3) else false;
    gate_neq_3:
        res_neq_3 <= true when (lhs_eq_3 /= rhs_eq_3) else false;

    -- LT / GT
    gate_lt_1:
        res_lt_1 <= true when (lhs_lt_1 < rhs_lt_1) else false;
    gate_gt_1:
        res_gt_1 <= true when (lhs_lt_1 > rhs_lt_1) else false;

    gate_lt_2:
        res_lt_2 <= true when (lhs_lt_2 < rhs_lt_2) else false;
    gate_gt_2:
        res_gt_2 <= true when (lhs_lt_2 > rhs_lt_2) else false;

    gate_lt_3:
        res_lt_3 <= true when (lhs_lt_3 < rhs_lt_3) else false;
    gate_gt_3:
        res_gt_3 <= true when (lhs_lt_3 > rhs_lt_3) else false;

    gate_lt_4:
        res_lt_4 <= true when (lhs_lt_4 < rhs_lt_4) else false;
    gate_gt_4:
        res_gt_4 <= true when (lhs_lt_4 > rhs_lt_4) else false;

    -- LEQ / GEQ
    gate_leq_1:
        res_leq_1 <= true when (lhs_leq_1 <= rhs_leq_1) else false;
    gate_geq_1:
        res_geq_1 <= true when (lhs_leq_1 >= rhs_leq_1) else false;

    gate_leq_2:
        res_leq_2 <= true when (lhs_leq_2 <= rhs_leq_2) else false;
    gate_geq_2:
        res_geq_2 <= true when (lhs_leq_2 >= rhs_leq_2) else false;

    gate_leq_3:
        res_leq_3 <= true when (lhs_leq_3 <= rhs_leq_3) else false;
    gate_geq_3:
        res_geq_3 <= true when (lhs_leq_3 >= rhs_leq_3) else false;

    gate_leq_4:
        res_leq_4 <= true when (lhs_leq_4 <= rhs_leq_4) else false;
    gate_geq_4:
        res_geq_4 <= true when (lhs_leq_4 >= rhs_leq_4) else false;

    test_ctrl_proc: process
    begin
        wait for 1 ns;

        -----------------------------------------------------------------------
        -- OR / NOR
        -----------------------------------------------------------------------
        lhs_or_1 <= true;
        wait for 1 ns;
        rhs_or_2 <= true;
        wait for 1 ns;

        -- Reset back
        lhs_or_1 <= false;
        wait for 1 ns;
        rhs_or_2 <= false;
        wait for 1 ns;

        -----------------------------------------------------------------------
        -- AND / NAND
        -----------------------------------------------------------------------
        lhs_and_1 <= true;
        wait for 1 ns;
        rhs_and_2 <= true;
        wait for 1 ns;

        -- Reset back
        lhs_and_1 <= false;
        wait for 1 ns;
        rhs_and_2 <= false;
        wait for 1 ns;

        -----------------------------------------------------------------------
        -- XOR / XNOR
        -----------------------------------------------------------------------
        lhs_xor_1 <= true;
        wait for 1 ns;
        rhs_xor_2 <= true;
        wait for 1 ns;

        -- Reset back
        lhs_xor_1 <= false;
        wait for 1 ns;
        rhs_xor_2 <= false;
        wait for 1 ns;

        -----------------------------------------------------------------------
        -- EQ / NEQ
        -----------------------------------------------------------------------
        -- EQ1 and EQ2 only init value covers 50 % of bins
        lhs_eq_3 <= true;
        wait for 1 ns;
        lhs_eq_3 <= false;
        wait for 1 ns;

        -----------------------------------------------------------------------
        -- LT / GT
        -----------------------------------------------------------------------
        lhs_lt_4  <= 1;
        wait for 1 ns;
        rhs_lt_4  <= 1;
        wait for 1 ns;
        lhs_lt_4  <= 5;
        wait for 1 ns;
        rhs_lt_4  <= 8;

        -----------------------------------------------------------------------
        -- LEQ / GEQ
        -----------------------------------------------------------------------
        lhs_leq_4  <= 1;
        wait for 1 ns;
        rhs_leq_4  <= 1;
        wait for 1 ns;
        lhs_leq_4  <= 5;
        wait for 1 ns;
        rhs_leq_4  <= 8;

        wait for 1 ns;
        wait;
    end process;

end architecture;

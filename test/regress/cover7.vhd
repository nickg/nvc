
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity cover7 is
end entity;

architecture test of cover7 is

    -- OR / NOR
    signal lhs_or_1   : std_logic := '0';
    signal rhs_or_1   : std_logic := '0';
    signal res_or_1   : std_logic;
    signal res_nor_1  : std_logic;

    signal lhs_or_2   : std_logic := '0';
    signal rhs_or_2   : std_logic := '0';
    signal res_or_2   : std_logic;
    signal res_nor_2  : std_logic;

    signal res_or_3   : std_logic;
    signal res_nor_3  : std_logic;

    -- AND / NAND
    signal lhs_and_1  : std_logic := '0';
    signal rhs_and_1  : std_logic := '0';
    signal res_and_1  : std_logic;
    signal res_nand_1 : std_logic;

    signal lhs_and_2  : std_logic := '1';
    signal rhs_and_2  : std_logic := '1';
    signal res_and_2  : std_logic;
    signal res_nand_2 : std_logic;

    signal res_and_3  : std_logic;
    signal res_nand_3 : std_logic;

    -- XOR / XNOR
    signal lhs_xor_1  : std_logic := '0';
    signal rhs_xor_1  : std_logic := '0';
    signal res_xor_1  : std_logic;
    signal res_xnor_1 : std_logic;

    signal lhs_xor_2  : std_logic := '0';
    signal rhs_xor_2  : std_logic := '0';
    signal res_xor_2  : std_logic;
    signal res_xnor_2 : std_logic;

    signal res_xor_3  : std_logic;
    signal res_xnor_3 : std_logic;

begin

    ---------------------------------------------------------------------------
    -- Logic ops on std_logic
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

    test_ctrl_proc: process
    begin
        wait for 1 ns;

        -----------------------------------------------------------------------
        -- OR / NOR
        -----------------------------------------------------------------------
        lhs_or_1 <= '1';
        wait for 1 ns;
        rhs_or_2 <= '1';
        wait for 1 ns;

        -- Reset back
        lhs_or_1 <= '0';
        wait for 1 ns;
        rhs_or_2 <= '0';
        wait for 1 ns;

        -----------------------------------------------------------------------
        -- AND / NAND
        -----------------------------------------------------------------------
        lhs_and_1 <= '1';
        wait for 1 ns;
        rhs_and_2 <= '1';
        wait for 1 ns;

        -- Reset back
        lhs_and_1 <= '0';
        wait for 1 ns;
        rhs_and_2 <= '0';
        wait for 1 ns;

        -----------------------------------------------------------------------
        -- XOR / XNOR
        -----------------------------------------------------------------------
        lhs_xor_1 <= '1';
        wait for 1 ns;
        rhs_xor_2 <= '1';
        wait for 1 ns;

        -- Reset back
        lhs_xor_1 <= '0';
        wait for 1 ns;
        rhs_xor_2 <= '0';
        wait for 1 ns;


        wait for 1 ns;
        wait;
    end process;

end architecture;

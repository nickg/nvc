
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity cover9 is
end entity;

architecture test of cover9 is

    -- OR / NOR
    signal lhs      : std_logic_vector(7 downto 0);
    signal rhs      : std_logic_vector(0 to 7);

    signal res_and  : std_logic_vector(7 downto 0);
    signal res_nand : std_logic_vector(7 downto 0);
    signal res_or   : std_logic_vector(7 downto 0);
    signal res_nor  : std_logic_vector(7 downto 0);
    signal res_xor  : std_logic_vector(7 downto 0);
    signal res_xnor : std_logic_vector(7 downto 0);

    signal invalid_lhs : std_logic_vector(5 downto 0);
    signal invalid_rhs : std_logic_vector(8 downto 0);
    signal invalid_res : std_logic_vector(8 downto 0);
    signal selector    : boolean := false;

begin

    res_and     <= lhs and  rhs;
    res_nand    <= lhs nand rhs;
    res_or      <= lhs or   rhs;
    res_nor     <= lhs nor  rhs;
    res_xor     <= lhs xor  rhs;
    res_xnor    <= lhs xnor rhs;

    test_process : process
    begin
        -- Bits 0, 7 in U to be sure nothing is covered on this bit

        -- Bit 1,6 : AND, NAND
        lhs(1) <= '0';
        rhs(6) <= '1';
        wait for 1 ns;

        lhs(1) <= '1';
        rhs(6) <= '1';
        wait for 1 ns;

        -- Bit 2, 5: OR, NOR
        lhs(2) <= '0';
        rhs(5) <= '0';
        wait for 1 ns;

        lhs(2) <= '0';
        rhs(5) <= '1';
        wait for 1 ns;

        -- Bit 3, 4: XOR, XNOR
        lhs(3) <= '0';
        rhs(4) <= '0';
        wait for 1 ns;

        lhs(3) <= '0';
        rhs(4) <= '1';
        wait for 1 ns;

        lhs(3) <= '1';
        rhs(4) <= '0';
        wait for 1 ns;

        lhs(3) <= '1';
        rhs(4) <= '1';
        wait for 1 ns;

        -- Remaining bits are placeholder for extensions

        wait;
    end process;

    invalid_proc: process
    begin
        if (selector) then
            -- Never executed since IEEE package throws fatal assert
            -- on mismatching lengths. Placed here to so that test
            -- will check that coverage is not generated for vectors
            -- of differetn lenghts.
            invalid_res <= invalid_lhs or invalid_rhs;
        else
            report "Skip invalid OP";
        end if;
        wait;
    end process;

end architecture;

library ieee;
use ieee.std_logic_1164.all;

entity sub_block is
    port (
        a       : in  std_logic;
        b       : in  std_logic;
        y       : out std_logic_vector(2 downto 0)
    );
end entity sub_block;

architecture test of sub_block is

begin

    xor_gate: y(0) <= a xor b;

    and_gate: process(a, b)
    begin
        if (a = '1' and b = '1') then
            y(1) <= '1';
        else
            y(1) <= '0';
        end if;
    end process;

    y(2) <= '0';

end architecture;


library ieee;
use ieee.std_logic_1164.all;

entity cover9 is
end entity;

architecture test of cover9 is

    signal s_a       : std_logic;
    signal s_b       : std_logic;
    signal s_y       : std_logic_vector(2 downto 0);

    signal a         : std_logic := 'X';
    signal b         : std_logic := 'X';
    signal y         : std_logic;

    signal a1        : std_logic;
    signal b1        : std_logic;
    signal y1        : std_logic;

    signal sel       : natural := 0;

    signal a2        : std_logic := '0';

begin

    -----------------------------------------------------------------------
    -- Test control
    -----------------------------------------------------------------------
    test_ctrl_proc: process
    begin
        -----------------------------------------------------------------------
        -- Leaves out uncovered in sub_block:
        --     - single bin from XOR
        --     - expression in "if" condition evaluated to true
        --     - Branch evalueated to true
        --     - Statement in "else"
        --     - Toggle on y[2].
        -----------------------------------------------------------------------
        s_a <= '1';
        wait for 1 ns;
        s_a <= '0';
        wait for 1 ns;
        s_b <= '1';
        wait for 1 ns;

        sel <= 1;
        wait for 1 ns;
        sel <= 2;
        wait for 1 ns;

        -----------------------------------------------------------------------
        -- Tests that excluding toggles works
        -----------------------------------------------------------------------
        a2 <= '1';
        wait for 1 ns;

        wait;
    end process;

    ---------------------------------------------------------------------------
    -- Tests that wildcard completion for the hierarchy:
    --      - Excludes everything in the hierarchy
    --      - Warns and does not exclude already covered items
    ---------------------------------------------------------------------------
    sub_block_inst : entity work.sub_block
    port map (
        a    => s_a,
        b    => s_b,
        y    => s_y
    );

    ---------------------------------------------------------------------------
    -- Tests that excluding bins 00,01,10,11 works
    ---------------------------------------------------------------------------
    P0: y <= a xor b;

    ---------------------------------------------------------------------------
    -- Tests that excluding statement and evaluated to True works
    ---------------------------------------------------------------------------
    and_gate: process(a1, b1)
    begin
        if (a1 = '1' and b1 = '1') then
            a: y1 <= '1';
        else
            b: y1 <= '0';
        end if;
    end process;

    ---------------------------------------------------------------------------
    -- Tests that excluding case choices bins and whole case choice works
    ---------------------------------------------------------------------------
    P1: process(sel)
    begin
        case_stmt: case sel is
        when 0 =>
            rpt1: report "Selector is 0";
        when 1 =>
            rpt2: report "Selector is 1";
        when 2 =>
            rpt3: report "Selector is 2";
        when 3 =>
            rpt4: report "Selector is 3";
        when 4 =>
            rpt5: report "Selector is 4";
        when others =>
            rpt6: report "Selector is OTHERS";
        end case;
    end process;

end architecture;

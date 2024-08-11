-------------------------------------------------------------------------------
-- Simple MUX sub-block
-------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;

entity sub_block is
    port (
        sel     : in  natural;
        output  : out std_logic_vector(7 downto 0)
    );
end entity;

architecture rtl of sub_block is

begin

    process (sel)
    begin
        case (sel) is
        when 0 => output <= x"AA";
        when 1 => output <= x"BB";
        when 2 => output <= x"CC";
        when 3 => output <= x"DD";
        when 4 => output <= x"00";
        when 5 => output <= x"11";
        when others => output <= x"EE";
        end case;
    end process;

end architecture;


-------------------------------------------------------------------------------
-- DUT TOP
-------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;

entity dut_top is
    port (
        clk     : in    std_logic;
        rst     : in    std_logic;

        start   : in    std_logic;
        done    : out   std_logic;
        output  : out   std_logic_vector(7 downto 0)
    );
end entity;

architecture rtl of dut_top is

    type t_fsm is (
        S_IDLE,
        S_1,
        S_2,
        S_DONE
    );

    signal curr_state   : t_fsm;

    signal sel          : natural;

begin

    process (rst, clk)
    begin
        if (rst) then
            curr_state <= S_IDLE;
        elsif (rising_edge(clk)) then
            case (curr_state) is
            when S_IDLE =>
                if (start = '1') then
                    curr_state <= S_1;
                end if;

            when S_1 =>
                curr_state <= S_2;

            when S_2 =>
                curr_state <= S_DONE;

            when S_DONE =>
                curr_state <= S_IDLE;
            end case;
        end if;
    end process;

    process (curr_state)
    begin
        sel <= 0;
        done <= '0';

        case (curr_state) is
        when S_IDLE =>
            sel <= 255;

        when S_1 =>
            sel <= 1;

        when S_2 =>
            sel <= 2;

        when S_DONE =>
            done <= '1';

        end case;
    end process;

    sub_block_inst : entity work.sub_block
    port map (
        sel     => sel,
        output  => output
    );

end architecture;

-------------------------------------------------------------------------------
-- TB for DUT
-------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;

entity cover23_top_tb is
end entity;

architecture tb of cover23_top_tb is

    signal clk : std_logic;
    signal rst : std_logic;

    signal start : std_logic := '0';
    signal done  : std_logic := '0';
    signal output : std_logic_vector(7 downto 0);

begin

    dut_top_inst : entity work.dut_top
    port map (
        clk     => clk,
        rst     => rst,

        start   => start,
        done    => done,
        output  => output
    );

    process
    begin
        clk <= '1';
        wait for 5 ns;
        clk <= '0';
        wait for 5 ns;
    end process;

    process
    begin
        rst <= '1';
        wait for 5 ns;
        rst <= '0';
        wait for 10 ns;

        start <= '1';
        wait for 10 ns;
        start <= '0';

        wait for 100 ns;
        std.env.finish;
    end process;

end architecture;


-------------------------------------------------------------------------------
-- TB for SUB-BLOCK
-------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;

entity cover23_unit_tb is
end entity;

architecture tb of cover23_unit_tb is

    signal sel : natural := 254;
    signal output : std_logic_vector(7 downto 0);

begin

    sub_block_inst : entity work.sub_block
    port map (
        sel     => sel,
        output  => output
    );

    test_proc : process
    begin
        wait for 5 ns;
        sel <= 3;
        wait for 5 ns;
        sel <= 4;
        wait for 5 ns;
        std.env.finish;
    end process;

end architecture;

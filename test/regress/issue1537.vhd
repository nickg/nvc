library ieee;
use ieee.std_logic_1164.all;

package issue1537_pkg is
    constant C_CLK_32M_PERIOD : time := 1 ns * 31.25;
end package;

library ieee;
use ieee.std_logic_1164.all;

entity issue1537_sub is
    generic (
        GC_CLK_PERIOD_32M : time
    );
    port (
        clk : in std_logic;
        rx_en : in std_logic
    );
end entity;

architecture rtl of issue1537_sub is
begin
    process(clk)
        constant C_STABLE_REQUIREMENT : time := GC_CLK_PERIOD_32M;
    begin
        if rising_edge(clk) then
            if rx_en'stable(C_STABLE_REQUIREMENT) then
                report "rx_en is stable";
            end if;
        end if;
    end process;
end architecture;

library ieee;
use ieee.std_logic_1164.all;
use work.issue1537_pkg.all;

entity issue1537 is
end entity;

architecture rtl of issue1537 is
    signal clk : std_logic := '0';
    signal rx_en : std_logic := '0';
begin
    clk <= not clk after 10 ns;
    
    process
    begin
        wait for 50 ns;
        rx_en <= '1';
        wait for 10 ns;
        rx_en <= '0';
        wait;
    end process;

    inst: entity work.issue1537_sub
        generic map (
            GC_CLK_PERIOD_32M => C_CLK_32M_PERIOD
        )
        port map (
            clk => clk,
            rx_en => rx_en
        );
end architecture;

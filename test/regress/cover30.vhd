library ieee;
use ieee.std_logic_1164.all;

entity cover30_sub is
    port (
        clk_in  : in  std_logic;
        clk_out : out std_logic
    );
end entity;

architecture rtl of cover30_sub is
begin
    clk_out <= clk_in;
end architecture;

library ieee;
use ieee.std_logic_1164.all;

entity cover30 is
end entity;

architecture test of cover30 is
    signal clk : std_logic := '0';
begin

    -- Port map requires a runtime signal connection rather than being
    -- eligible for direct nexus aliasing, which is what previously
    -- triggered the toggle coverage callback to run before the port
    -- was wired up to its driver
    i_sub : entity work.cover30_sub
        port map (
            clk_in  => clk,
            clk_out => open
        );

    clk <= not clk after 5 ns;

    process
    begin
        wait for 25 ns;
        std.env.finish;
    end process;

end architecture;

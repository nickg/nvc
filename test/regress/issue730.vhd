library ieee;
use ieee.std_logic_1164.all;


entity module_ref_label is
    generic (
        G_PATH : string := "None"
    );
    port (
        clk   : in std_logic;
        print : in std_logic
    );
end entity module_ref_label;

architecture rtl of module_ref_label is

begin
    process (clk)
    begin
        if rising_edge(clk) then
            if print = '1' then
                report G_PATH;
            end if;
        end if;
    end process;

end architecture;

-------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use std.env.finish;

entity issue730 is
end entity issue730;

architecture sim of issue730 is
    signal clk : std_logic := '0';
    signal print : std_logic;
begin
    clk <= not clk after 5 ns;

    test_mod : entity work.module_ref_label
    generic map (
        G_PATH => test_mod'path_name
    )
    port map(
        clk => clk,
        print => print
    );

    test_proc : process is
    begin
        report test_proc'path_name;
        wait until rising_edge(clk);
        print <= '1';
        wait until rising_edge(clk);
        print <= '0';
        finish;
        wait;
    end process;
end architecture;

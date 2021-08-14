library ieee;
use ieee.std_logic_1164.all;

entity sub is
    port (
        clk : out std_logic;
        cnt : inout integer := 0);
end entity;

architecture test of sub is
    signal clk_i   : bit := '0';
    signal clk_std : std_logic;
begin

    clk_i <= not clk_i after 1 ns;

    clk_std <= to_stdulogic(clk_i);

    clk <= clk_std;

    process (clk_std) is
    begin
        if rising_edge(clk_std) then
            cnt <= cnt + 1;
        end if;
    end process;

end architecture;

-------------------------------------------------------------------------------

entity ieee2 is
end entity;

library ieee;
use ieee.std_logic_1164.all;

architecture test of ieee2 is
    signal cnt : integer := 0;
    signal clk : std_logic;
begin

    sub_i: entity work.sub port map ( clk, cnt );

    process (clk) is
    begin
        if rising_edge(clk) then
            report "clock!";
        end if;
    end process;

    process is
    begin
        wait for 10 ns;
        report integer'image(cnt);
        assert cnt = 5;
        wait;
    end process;

end architecture;

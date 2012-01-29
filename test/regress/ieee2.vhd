library ieee;
use ieee.std_logic_1164.all;

entity sub is
    port (
        clk : in std_logic;
        cnt : out integer );
end entity;

architecture test of sub is
begin

    process (clk) is
    begin
        if rising_edge(clk) then
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
    signal clk : std_logic := '0';
begin

    clk <= not clk after 1 ns;

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

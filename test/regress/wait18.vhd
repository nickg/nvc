entity wait18 is
end entity;

library ieee;
use ieee.std_logic_1164.all;

architecture test of wait18 is
    signal clk, d, q : std_logic := '0';
begin

    process (clk) is
    begin
        if rising_edge(clk) then
            q <= d;
        end if;
    end process;

    process is
    begin
        clk <= '1' after 10 ns;
        d <= '1';
        wait for 11 ns;
        assert q = '1';
        wait;
    end process;

    postponed process (clk) is
    begin
        if rising_edge(clk) then
            assert q = '1';
        end if;
    end process;

end architecture;

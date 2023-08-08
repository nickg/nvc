library ieee;
use ieee.std_logic_1164.all;

entity sub is
    port ( i0 : in std_logic_vector(4 downto 0) := "LLLLL";
           o0 : out std_logic_vector(1 downto 0) );
end entity;

architecture test of sub is
begin

    p1: process is
    begin
        report to_string(i0);
        assert i0 = "LU0L1";
        o0 <= "11";
        wait for 1 ns;
        assert i0 = "L11L1";
        wait;
    end process;

end architecture;

-------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;

entity open1 is
end entity;

architecture test of open1 is
    signal x : std_logic_vector(1 downto 0) := "Z0";
begin

    u: entity work.sub
        port map (
            i0(4) => open,
            i0(3 downto 2) => x,
            i0(0) => '1',
            o0(1) => x(1),
            o0(0) => open );

    p1: process is
    begin
        x <= "Z1";
        wait;
    end process;

end architecture;

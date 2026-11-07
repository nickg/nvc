library ieee;
use ieee.std_logic_1164.all;

package comps is
    component sub10 is
        generic (MODE  : string  := "auto";
                 SCALE : real    := 1.0;
                 WIDTH : integer := 4;
                 NAME  : string  := "def");
        port (o : out std_logic_vector(31 downto 0));
    end component;
end package;

entity mixed10 is
end entity;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.comps.all;

architecture test of mixed10 is
    signal o1, o2 : std_logic_vector(31 downto 0);
begin
    -- String, real, and integer generics with some left at their default
    u1: component sub10 generic map (MODE => "fast", SCALE => 2.5)
        port map (o1);
    u2: component sub10 generic map (WIDTH => 7) port map (o2);

    check: process is
    begin
        wait for 1 ns;
        assert to_integer(unsigned(o1)) = 1000 + 250 + 4 + 3
            report to_hstring(o1);
        assert to_integer(unsigned(o2)) = 2000 + 100 + 7 + 3
            report to_hstring(o2);
        report "PASSED";
        wait;
    end process;
end architecture;

library ieee;
use ieee.std_logic_1164.all;

package pack is

    type rec is record
        f : std_logic_vector;
    end record;

    subtype rec4 is rec(f(1 to 4));
end package;

-------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;

use work.pack.all;

entity record31 is
    port ( r : inout rec4 );
end entity;

architecture test of record31 is
begin

    p1: process is
    begin
        r.f <= "ZZZZ";
        wait for 1 ns;
        assert r.f = "1010";
        r.f <= "Z1ZZ";
        wait for 1 ns;
        assert r.f = "1X10";
        wait;
    end process;

    p2: process is
    begin
        r.f <= "1010";
        wait for 1 ns;
        assert r.f = "1010";
        wait for 1 ns;
        assert r.f = "1X10";
        wait;
    end process;

end architecture;

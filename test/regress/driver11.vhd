library ieee;
use ieee.std_logic_1164.all;

entity sub is
    port ( x : inout std_logic );
end entity;

architecture test of sub is
begin

    p1: process is
    begin
        x <= '1';
        wait for 1 ns;
        x <= '0';
        wait for 0 ns;
        assert x'active;
        assert not x'event;
        assert x = 'U';
        x <= 'L';
        wait for 0 ns;
        assert x'active;
        assert not x'event;
        assert x = 'U';
        assert x = x'last_value;
        wait;
    end process;

end architecture;

-------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;

entity driver11 is
end entity;

architecture test of driver11 is
    signal y : std_logic;
begin

    u: entity work.sub port map ( y );

    p2: process is
    begin
        y <= 'U';
        wait;
    end process;

end architecture;

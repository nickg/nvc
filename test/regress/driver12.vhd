library ieee;
use ieee.std_logic_1164.all;

package pack is
    type rec is record
        x : std_logic;
        y : std_logic_vector(2 downto 0);
    end record;

    function init_signals return rec;
end package;

package body pack is
    function init_signals return rec is
    begin
        return (x => 'Z', y => "000");
    end function;
end package body;

-------------------------------------------------------------------------------

use work.pack.all;

entity sub is
    port ( p : inout rec := init_signals );
end entity;

library ieee;
use ieee.std_logic_1164.all;

architecture test of sub is
begin
    p1: process is
    begin
        assert p.x = '0';
        assert p.y = "000";
        wait for 1 ns;
        assert p.x = '1';
        wait;
    end process;
end architecture;

-------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;

entity sub2 is
    port ( x : out std_logic := '0';
           y : in std_logic_vector(2 downto 0) );
end entity;

architecture test of sub2 is
begin
    x <= '0', '1' after 1 ns;
end architecture;

-------------------------------------------------------------------------------

entity driver12 is
end entity;

library ieee;
use ieee.std_logic_1164.all;

architecture test of driver12 is
    signal s : std_logic;
    signal t : std_logic_vector(2 downto 0);
begin

    u1: entity work.sub
        port map (
            p.x => s,
            p.y => t );

    u2: entity work.sub2
        port map (
            x => s,
            y => t );

end architecture;

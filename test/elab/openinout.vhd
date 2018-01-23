package p is
    function func(x : natural) return natural;
end package;

package body p is
    function func(x : natural) return natural is
    begin
        return x + 1;
    end function;
end package body;

-------------------------------------------------------------------------------

entity sub is
    port ( x : inout natural;
           x2 : inout integer := 5;
           y : out natural;
           y2 : out integer );
end entity;

use work.p.all;

architecture test of sub is
begin
    y <= func(x);                       -- Crash here reading open inout port
    y2 <= func(x2);
end architecture;

-------------------------------------------------------------------------------

entity sub2 is
    port ( x : inout natural;
           y : out natural );
end entity;

architecture test of sub2 is
begin
    sub_i: entity work.sub port map ( x => x, y => y, y2 => open );
end architecture;

-------------------------------------------------------------------------------

entity top is
end entity;

architecture test of top is
    signal y, y2, y3 : natural;
begin
    uut: entity work.sub port map ( x => open, y => y, y2 => y2 );
    uut2: entity work.sub2 port map ( y => y3 );
end architecture;

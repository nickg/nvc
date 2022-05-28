package pack is
    procedure check (i, o : integer);
    procedure debug (i, o : integer);
end package;

package body pack is
    procedure check (i, o : integer) is
    begin
        assert i < o;
    end procedure;

    procedure debug (i, o : integer) is
    begin
        report "i=" & integer'image(i) & " o=" & integer'image(o);
    end procedure;
end package body;

-------------------------------------------------------------------------------

use work.pack.all;

entity sub is
    port ( i : in integer;
           o : out integer := integer'right );
begin
    debug(i, o);
    postponed check(i, o);
end entity;

architecture test of sub is
begin
    o <= i + 1;
end architecture;

-------------------------------------------------------------------------------

entity proc12 is
end entity;

architecture test of proc12 is
    signal i, o : integer;
begin

    u: entity work.sub port map (i, o);

    p1: process is
    begin
        i <= 5;
        wait for 1 ns;
        assert o = 6;
        wait;
    end process;

end architecture;

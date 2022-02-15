package pack is
    type rec is record
        x, y : natural;
    end record;

    function rec_to_int (r : rec) return natural;
    function int_to_rec (x : natural) return rec;
end package;

package body pack is
    function rec_to_int (r : rec) return natural is
    begin
        return r.x + r.y;
    end function;

    function int_to_rec (x : natural) return rec is
    begin
        return (x / 2, x * 2);
    end function;
end package body;

-------------------------------------------------------------------------------

use work.pack.all;

entity sub is
    port (
        i1 : in rec;
        i2 : in natural );
end entity;

architecture test of sub is
begin

    p1: process is
    begin
        assert i1 = ( 0, 0 );
        assert i2 = 0;
        wait for 0 ns;
        assert i1 = ( 3, 12 );
        assert i2 = 5;
        wait for 2 ns;
        assert i2 = 8;
        wait;
    end process;

end architecture;

-------------------------------------------------------------------------------

use work.pack.all;

entity conv7 is
end entity;

architecture test of conv7 is
    signal s1 : natural;
    signal s2 : rec;
begin

    uut: entity work.sub
        port map (
            i1 => int_to_rec(s1),
            i2 => rec_to_int(s2) );

    main: process is
    begin
        s1 <= 6;
        s2 <= (2, 3);
        wait for 1 ns;
        s2.y <= 6;
        wait;
    end process;

end architecture;

package pack is
    type rec_t is record
        x, y : natural;
    end record;

    view in_view of rec_t is
        x : in;
        y : out;
    end view;

    alias out_view is in_view'converse;
end package;

-------------------------------------------------------------------------------

use work.pack.all;

entity sub is
    port ( i : view in_view; o : view out_view );
end entity;

architecture test of sub is
begin

    o.x <= i.x + 1;
    i.y <= o.y + 5;

end architecture;

-------------------------------------------------------------------------------

use work.pack.all;

entity view2 is
end entity;

architecture test of view2 is
    signal i, o : rec_t;
begin

    u: entity work.sub port map (i, o);

    check: process is
    begin
        assert i = (0, 0);
        assert o = (0, 0);
        wait for 0 ns;
        assert i = (0, 5);
        assert o = (1, 0);
        i.x <= 7;
        o.y <= 12;
        wait for 1 ns;
        assert i = (7, 17);
        assert o = (8, 12);
        wait;
    end process;

end architecture;

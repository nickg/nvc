package pack is
    type rec_t is record
        x, y : natural;
    end record;

    view in_view of rec_t is
        x : in;
        y : out;
    end view;

    view out_view of rec_t is
        x : out;
        y : in;
    end view;
end package;

-------------------------------------------------------------------------------

use work.pack.all;

entity view6 is
end entity;

architecture test of view6 is

    procedure update1 ( signal i : view in_view;
                        signal o : view out_view ) is
    begin
        o.x <= i.x + 1;
        i.y <= o.y + 5;
    end procedure;

    procedure update2 ( signal i : view in_view'converse;
                        signal o : view out_view'converse ) is
    begin
        i.x <= 7;
        o.y <= 12;
    end procedure;

    signal i, o : rec_t;
begin

    u1: update1(i, o);

    u2: process is
    begin
        wait for 0 ns;
        update2(i, o);
        wait;
    end process;

    check: process is
    begin
        assert i = (0, 0);
        assert o = (0, 0);
        wait for 0 ns;
        assert i = (0, 5);
        assert o = (1, 0);
        wait for 1 ns;
        assert i = (7, 17);
        assert o = (8, 12);
        wait;
    end process;

end architecture;

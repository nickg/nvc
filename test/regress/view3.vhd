package pack is
    type rec_t is record
        x, y : natural;
        b : bit_vector;
    end record;

    view in_view of rec_t is
        x : in;
        y : out;
        b : in;
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
    o.b <= not i.b;

end architecture;

-------------------------------------------------------------------------------

use work.pack.all;

entity sub2 is
    port ( i : view in_view; o : view out_view );
end entity;

architecture test of sub2 is
begin

    u: entity work.sub port map ( i, o );

end architecture;

-------------------------------------------------------------------------------

use work.pack.all;

entity view3 is
end entity;

architecture test of view3 is
    signal i, o : rec_t(b(1 to 3));
begin

    u: entity work.sub port map (i, o);

    check: process is
    begin
        assert i = (0, 0, "000");
        assert o = (0, 0, "000");
        wait for 0 ns;
        assert i = (0, 5, "000");
        assert o = (1, 0, "111");
        i.x <= 7;
        o.y <= 12;
        i.b <= "101";
        wait for 1 ns;
        assert i = (7, 17, "101");
        assert o = (8, 12, "010");
        wait;
    end process;

end architecture;

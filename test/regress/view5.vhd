package pack is
    type rec_t is record
        x, y : natural;
    end record;

    type rec_array_t is array (natural range <>) of rec_t;

    view in_view of rec_t is
        x : in;
        y : out;
    end view;

    alias out_view is in_view'converse;
end package;

-------------------------------------------------------------------------------

use work.pack.all;

entity sub is
    port ( i : view (in_view) of rec_array_t(0 to 0);
           o : view (out_view) of rec_array_t(0 to 0) );
end entity;

architecture test of sub is
begin

    o(0).x <= i(0).x + 1;
    i(0).y <= o(0).y + 5;

end architecture;

-------------------------------------------------------------------------------

use work.pack.all;

entity view5 is
end entity;

architecture test of view5 is
    signal i, o : rec_array_t(0 to 0);
begin

    u: entity work.sub port map (i, o);

    check: process is
    begin
        assert i(0) = (0, 0);
        assert o(0) = (0, 0);
        wait for 0 ns;
        assert i(0) = (0, 5);
        assert o(0) = (1, 0);
        i(0).x <= 7;
        o(0).y <= 12;
        wait for 1 ns;
        assert i(0) = (7, 17);
        assert o(0) = (8, 12);
        wait;
    end process;

end architecture;

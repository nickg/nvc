entity alias14 is
end entity;

architecture test of alias14 is
    type point is record
        x, y : integer;
    end record;

    type point_vec is array (natural range <>) of point;

    type sig_type is record
        points : point_vec;
    end record;

    signal s1 : sig_type(points(1 to 4));
    alias s1p1x is s1.points(1).x;

    type nest_type is record
        sub : sig_type;
    end record;

    signal s2 : nest_type(sub(points(1 to 2)));
    alias s2p is s2.sub.points;
begin

    p1: process is
    begin
        s1.points <= ((1, 2), (3, 4), (5, 6), (7, 8));
        wait for 1 ns;
        assert s1p1x = 1;
        s1p1x <= 5;
        wait for 1 ns;
        assert s1.points = ((5, 2), (3, 4), (5, 6), (7, 8));

        s2p <= ((1, 2), (3, 4));
        wait for 1 ns;
        assert s2p(1).x = 1;
        assert s2.sub.points(2).y = 4;

        wait;
    end process;

end architecture;

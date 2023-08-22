entity conv9 is
end entity;

architecture test of conv9 is
    type t_rec1 is record
        x, y : integer;
    end record;

    type t_rec2 is record
        y, x : integer;
    end record;

    type t_rec3 is record
        x : t_rec1;
        y : integer_vector(1 to 3);
    end record;

    type t_rec4 is record
        y : integer_vector;
        x : t_rec2;
    end record;

begin

    p1: process is
        variable r1 : t_rec1;
        variable r2 : t_rec2;
        variable r3 : t_rec3;
        variable r4 : t_rec4(y(1 to 3));
    begin
        r1 := (1, 2);
        r2 := t_rec2(r1);
        assert r2 = (2, 1);
        r3 := (x => (4, 5), y => (1, 2, 3));
        r4 := t_rec4(r3);
        assert r4.x = (5, 4);
        assert r4.y = (1, 2, 3);
        r4 := ((7, 8, 9), (6, 5));
        r3 := t_rec3(r4);
        assert r3.x = (5, 6);
        assert r3.y = (7, 8, 9);
        wait;
    end process;

end architecture;

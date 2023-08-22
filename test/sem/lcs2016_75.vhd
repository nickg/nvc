entity lcs2016_75 is
end entity;

architecture test of lcs2016_75 is
    type t_rec1 is record
        x, y : integer;
    end record;

    type t_rec2 is record
        y, x : integer;
    end record;

    type t_rec3 is record
        x, z : integer;
        y    : real;
    end record;

    type t_rec4 is record
        x : t_rec1;
        y : t_rec2;
    end record;

    type t_rec5 is record
        x : t_rec2;
        y : t_rec1;
    end record;
begin

    p1: process is
        variable r1 : t_rec1;
        variable r2 : t_rec2;
        variable r3 : t_rec3;
        variable r4 : t_rec4;
        variable r5 : t_rec5;
    begin
        r1 := t_rec1(r2);               -- OK
        r1 := t_rec1(r3);               -- OK
        r3 := t_rec3(r1);               -- Error
        r4 := t_rec4(r5);               -- OK
        wait;
    end process;

end architecture;

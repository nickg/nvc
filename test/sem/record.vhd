package p is

    type r1 is record                   -- OK
        x : integer;
        y : integer;
    end record;

    type r2 is record                   -- Error
        x, x : integer;
    end record;

    type r3;

    type r3 is record                   -- Error
        x : r3;
    end record;

    type r4 is record
        x, y, z : integer;
    end record;

    type r5 is record
        x : r1;
        y : integer;
    end record;

    type r1_vec is array (integer range <>) of r1;

    type r6 is record
        x : r1_vec;                     -- Error
    end record;

end package;

package body p is

    procedure p1 is
        variable v1 : r1 := (1, 2);
        variable v2 : r4 := (1, 2);        -- Error
        variable v3 : r1 := (1, v1);       -- Error
        variable v4 : r1 := (x => 1, y => 2);
        variable v5 : r1 := (x => 1);   -- Error
        variable v6 : r1 := (x => 1, y => 2, q => 1);  -- Error
        variable v7 : r1 := (x => 1, y => v1);  -- Error
        variable v8 : r1 := (others => 9);
        variable v9 : r1 := (x => 1, others => 2);
        variable v10 : r1 := (x => 1, x => 2, y => 3);  -- Error
        variable v11 : r1 := (1, x => 4, y => 2);  -- Error
        variable v12 : r1 := (1, y => 4);
        variable v13 : r1;
    begin
    end procedure;

    procedure p2 is
        variable v1 : r1;
        variable v2 : r5;
    begin
        v1.x := 2;
        v1.y := v1.x + 5;
        v2.x.x := 3;
    end procedure;

    procedure p3 is
        variable a1 : r1_vec;           -- Error
    begin
    end procedure;

    procedure p4 is
        variable a2 : r1_vec(0 to 3);   -- OK
    begin
        a2(2).x := 5;                   -- OK
        a2(1).f := 2;                   -- Error
        a2(0).x := a2(1).y;             -- OK
    end procedure;

end package body;

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

end package;

package body p is

    procedure p is
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
    begin
    end procedure;

end package body;

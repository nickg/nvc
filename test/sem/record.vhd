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

end package;

package layout is

    type r1 is record
        x : integer;
        y : boolean;
        z : real;
    end record;

    type r2 is record
        x : r1;
        y : integer;
    end record;

    subtype a is string(1 to 5);

end package;

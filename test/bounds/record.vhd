entity e is
end entity;

architecture a of e is
    type r1 is record
        x, y : integer;
    end record;

    type r2 is record
        x, y : integer;
    end record;

    constant c1 : r1 := (x => 1, y => 2);  -- OK
    constant c2 : r1 := (1, 2);         -- OK
    constant c3 : r1 := (x => 1);       -- Error
begin
end architecture;

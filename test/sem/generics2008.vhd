entity e1 is
    generic (
        a : integer;
        b : integer := a;               -- OK
        c : integer := d;               -- Error
        d : integer;
        e, f : integer := e             -- Error
        );
end entity;

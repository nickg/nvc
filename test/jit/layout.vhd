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

    type t_byte is array (1 to 8) of bit;
    type t_byte_vector is array (natural range <>) of t_byte;

    constant c1 : t_byte_vector(1 to 3) := (others => X"00");

end package;

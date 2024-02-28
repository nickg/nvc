package cons1 is
    type t_rec1 is record
        f1 : string;
        f2 : string(-1 to 0);           -- Error
    end record;

    constant c1 : t_rec1(f1(0 to 4)) := (f1 => "hello", f2 => "12");  -- Error

    subtype t_string_sub is string(0 to 5);  -- Error
    subtype t_sub_sub is t_string_sub;  -- OK

    type t_array is array (natural range <>) of string(0 to 3);  -- Error

    constant c2 : t_array'element;      -- OK
    constant c3 : t_array'element;      -- OK
end package;

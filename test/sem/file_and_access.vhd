package p is
    type t_int_file     is file of integer;
    type t_int_access   is access integer;
    type t_access_array is array (0 to 1) of t_int_access;
    type t_access_record is record
        a   : integer;
        b   : t_int_access;
    end record;

    constant c1 : t_int_access;     -- Error
    constant c2 : t_access_array;   -- Error
    constant c3 : t_access_record;  -- Error
    constant c4 : t_int_file;       -- Error

    signal s1 : t_int_access;       -- Error
    signal s2 : t_access_array;     -- Error
    signal s3 : t_access_record;    -- Error
    signal s4 : t_int_file;         -- Error

    attribute a1 : t_int_access;    -- Error
    attribute a2 : t_access_array;  -- Error
    attribute a3 : t_access_record; -- Error
    attribute a4 : t_int_file;      -- Error

end package p;

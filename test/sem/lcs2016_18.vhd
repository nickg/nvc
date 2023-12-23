entity lcs2016_18 is
end entity;

architecture test of lcs2016_18 is
    type int_ptr is access integer;
    constant c1 : int_ptr'designated_subtype := 2;  -- OK
    constant c2 : test'designated_subtype := 1;  -- Error
    constant c3 : integer'designated_subtype := 2;  -- Error
    type file_of_int is file of integer;
    constant c4 : file_of_int'designated_subtype := 2;  -- OK
begin
    p1: process is
        variable v1 : int_ptr;
        constant c5 : v1'designated_subtype := 5;  -- OK
        constant c6 : v1'designated_subtype := true;  -- Error
    begin
    end process;

    p2: process is
        type int_map is array (integer range <>, boolean range <>) of integer;
        constant c7 : int_map'index := 23;  -- OK
        constant c8 : int_map'index(2) := true;  -- OK
        constant c9 : int_map'index(2) := 12;  -- Error
        constant c10 : int_map'index(0) := 55;  -- Error
        constant c11 : int_map'index(3) := false;  -- Error
        constant c12 : integer'index := 1;   -- Error
        variable v2 : int_map(1 to 3, true to false);
        constant c13 : v2'index(1) := 5;  -- OK
        constant c14 : int_map'index(1 + 0) := 3;  -- OK (but not supported)
    begin
    end process;

    p3: process is
        constant x : integer := 2;
        variable y : real;
    begin
        report x'image;                 -- OK
        report p3'image;                -- Error
        assert x'left = integer'left;   -- OK
        report y'length;                -- Error
        report file_of_int'image(3);    -- Error
    end process;

end architecture;

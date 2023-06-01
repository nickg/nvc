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

end architecture;

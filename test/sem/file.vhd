package p is

    type ft is file of natural;         -- OK

    type t_prot is protected
    end protected;

    type int_ptr is access integer;
    type bad1 is file of int_ptr;       -- Error

    type bad2 is file of ft;            -- Error

    type bad3 is file of t_prot;        -- Error

    file f1 : ft is "foo.txt" ;         -- OK

    file f2 : integer is "bar.txt";     -- Error

    file f3 : ft open READ_MODE is "x.txt";  -- OK

    file f4 : ft open 5 is "y.txt";     -- Error

    file f5 : ft;                       -- OK

    file f6 : ft is 6;                  -- Error

    type arr_1d is array (natural range <>) of integer;
    type arr_2d is array (natural range <>, natural range <>) of integer;
    subtype subarr_1d is arr_1d (1 to 2);
    subtype subarr_2d is arr_2d (1 to 2, 3 to 4);

    type ft2  is file of arr_1d;         -- OK
    type bad4 is file of arr_2d;         -- Error
    type ft3  is file of subarr_1d;      -- OK
    type bad5 is file of subarr_2d;      -- Error

end package;

package body p is

    procedure test is
        variable status : file_open_status;
        variable n      : natural;
    begin
        file_open(f5, "foo.txt", WRITE_MODE);  -- OK
        file_open(f5, "bar.txt");       -- OK
        file_open(status, f5, "x.txt");  -- OK
        file_close(f1);                 -- OK
        write(f1, 5);              -- OK
        read(f1, n);               -- OK
        read(f1, status);          -- Error
        assert endfile(f1);             -- OK
    end procedure;

    type t_prot is protected body
    end protected body;

end package body;

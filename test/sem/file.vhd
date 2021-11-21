package p is

    type ft is file of natural;         -- OK

    type int_ptr is access integer;
    type bad1 is file of int_ptr;       -- Error

    type bad2 is file of ft;            -- Error

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

    type t_ptr_arr is array (natural range <>) of int_ptr;
    subtype sub_ptr_arr is t_ptr_arr (1 to 2);

    type    t_rec is record
                a   : integer;
                b   : int_ptr;
            end record;

    type    t_rec2 is record
                a   : integer;
                b   : sub_ptr_arr;
                c   : real;
            end record;

    type bad6 is file of t_ptr_arr;         -- Error
    type bad7 is file of sub_ptr_arr;       -- Error
    type bad8 is file of t_rec;             -- Error
    type bad9 is file of t_rec2;            -- Error

    type    t_ok_rec is record
                a   : integer;
                b   : time;
                c   : arr_1d(1 to 1);
                d   : arr_2d(10 to 20, 1 to 3);
            end record;

    type ft4 is file of t_ok_rec;           -- OK

    function get_ft4 return ft4;        -- Error

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

end package body;

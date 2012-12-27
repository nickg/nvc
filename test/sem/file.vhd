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
        file_write(f1, 5);              -- OK
        file_read(f1, n);               -- OK
        file_read(f1, status);          -- Error
        assert endfile(f1);             -- OK
    end procedure;

end package body;

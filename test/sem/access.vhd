package p is

    type int_ptr is access integer;     -- OK

    type bad1 is access foo;            -- Error

end package;

package body p is

    procedure test is
        variable v : int_ptr;
        variable i : integer;
    begin
        v := null;                      -- OK
        i := null;                      -- Error
        deallocate(v);                  -- OK
    end procedure;

end package body;

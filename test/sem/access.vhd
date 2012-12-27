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
        v := new integer;               -- OK
        v := new integer'(5);           -- OK
        v := new 5;                     -- Error
        v := new i;                     -- Error
        v.all := 5;                     -- OK
        v := 5;                         -- Error
        i := v.all + 5;                 -- OK
    end procedure;

end package body;

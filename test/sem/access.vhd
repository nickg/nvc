package p is

    type int_ptr is access integer;     -- OK

    type bad1 is access foo;            -- Error

    type rec;

    type rec_ptr is access rec;

    type rec is record
        value : integer;
        link  : rec_ptr;
    end record;

end package;

package body p is

    procedure test is
        variable v : int_ptr;
        variable i : integer;
        variable r : rec_ptr;
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
        r := new rec;                   -- OK
        r.all.value := 1;               -- OK
        r.value := 1;                   -- OK
        r.link := r;                    -- OK
        r.link := r.all;                -- Error
        i := r.value;                   -- OK
        r := r.all.link;                -- OK
    end procedure;

end package body;

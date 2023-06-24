package p is

    type int_ptr is access integer;     -- OK

    type bad1 is access foo;            -- Error

    type rec;

    type rec_ptr is access rec;

    type rec is record
        value : integer;
        link  : rec_ptr;
    end record;

    type int_vec is array (integer range <>) of integer;

    type int_vec_ptr is access int_vec;

    type string_ptr is access string;

end package;

package body p is

    procedure test is
        variable v : int_ptr;
        variable i : integer;
        variable r : rec_ptr;
        variable a : int_vec_ptr;
        variable s : string_ptr;
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
        a := new int_vec(1 to 3);       -- OK
        a.all(5) := 2;                  -- OK
        a(5) := 2;                      -- OK
        a(1 to 2) := (1, 2);            -- OK
        s := new string'("");           -- OK
        s := new integer'(1);           -- Error
        s := new s(1 to 3);             -- Error
    end procedure;

    procedure test2(x : inout rec_ptr) is
    begin
        x.value := x.value + 1;
    end procedure;

    procedure test3 is
        type a;
        type a is access integer;       -- OK
        variable v : a;                 -- OK
    begin
    end procedure;

    type int_ptr_array is array (integer range <>) of int_ptr;
    type int_ptr_array_ptr is access int_ptr_array;

    procedure alloc_ptr_array(x : out int_ptr_array_ptr) is
    begin
        x := new int_ptr_array;          -- Error
        x := new int_ptr_array(1 to 3);  -- OK
        x.all := (null, null, null);     -- OK
    end procedure;

    procedure tets4 is
        type bvp is access bit_vector;
        variable x : bvp(1 to 4) := new bit_vector'("1010");  -- OK
        variable y : int_ptr(1 to 3) := int_ptr'(null);  -- Error
    begin
    end procedure;

    procedure test5 is
        type foo;
        variable f : foo;               -- Error
    begin
    end procedure;

    procedure test6 is
        variable v : int_vec(1 to 3);
    begin
        v(1) := new integer'(5);        -- Error
    end procedure;

    procedure test7 is
        type a;
        type a_ptr is access a;
        variable p : a_ptr;
    begin
        p := new a;                     -- Error
    end procedure;

    procedure test8 is
        variable p : int_vec(1 to 3);
    begin
        for i in p.all'range loop       -- Error
        end loop;
    end procedure;

    procedure test9 is
        type pt is protected
            procedure dummy;
        end protected;
        type pt is protected body
            procedure dummy is
            begin
            end procedure;
        end protected body;

        type ptp is access pt;          -- Error
        type ft is file of natural;     -- OK
        type ftp is access ft;          -- Error
    begin
    end procedure;

end package body;

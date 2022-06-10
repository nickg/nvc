package access1 is

    type int_ptr is access integer;

    procedure deref (variable p : int_ptr; result : out integer);
    procedure test1 (variable p : inout int_ptr);
    procedure oom;
    procedure gc_a_lot;

end package;

package body access1 is

    procedure deref (variable p : int_ptr; result : out integer) is
    begin
        result := p.all;
    end procedure;

    procedure test1 (variable p : inout int_ptr) is
        variable i : integer;
    begin
        p := new integer;
        p.all := 5;
        deref(p, i);
        assert i = 5;
        deallocate(p);
    end procedure;

    type biglist;

    type biglist_ptr is access biglist;

    type biglist is record
        str : string(1 to 2 ** 10);
        chain : biglist_ptr;
    end record;

    procedure oom is
        variable head, tail : biglist_ptr;
    begin
        head := new biglist;
        tail := head;
        while true loop
            tail.chain := new biglist;
            tail := tail.chain;
        end loop;
    end procedure;

    procedure gc_a_lot is
        variable tail : biglist_ptr;
    begin
        for i in 1 to 5000 loop
            tail := new biglist;
        end loop;
    end procedure;

end package body;

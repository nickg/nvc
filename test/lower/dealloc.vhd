package pack is
    type ptr is access integer;
    procedure dealloc(p : inout ptr);
end package;

package body pack is

    procedure another_proc(p : inout ptr) is
    begin
        deallocate(p);
    end procedure;

    procedure dealloc(p : inout ptr) is
    begin
        another_proc(p);
    end procedure;
end package body;

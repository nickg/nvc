package vunit3 is
    type rec;
    type rec_ptr is access rec;

    impure function alloc_reg return rec_ptr;

end package;

package body vunit3 is

    type rec is record
        x : integer;
    end record;

    impure function alloc_reg return rec_ptr is
    begin
        return new rec;                 -- Crash lowering here
    end function;

end package body;

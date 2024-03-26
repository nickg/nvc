package vunit4 is
    type rec;
    type int_ptr is access integer;

    type rec is record
        x : int_ptr;
    end record;

    impure function get_rec(n : natural) return rec;

end package;

package body vunit4 is

    type int_ptr_vec is array (natural range <>) of int_ptr;

    shared variable v : int_ptr_vec(1 to 5);

    impure function get_rec(n : natural) return rec is
    begin
        return rec'(x => v(n));
    end function;

end package body;

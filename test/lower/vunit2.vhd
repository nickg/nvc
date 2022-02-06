package vunit2 is
    type int_ptr is access integer;
    type array_of_access is array (natural range <>) of int_ptr;

    impure function get_one (idx : natural) return int_ptr;
end package;

package body vunit2 is

    shared variable a : array_of_access(1 to 5);

    impure function get_one (idx : natural) return int_ptr is
    begin
        return a(idx);
    end function;

end package body;

package deferred is
    type t_int_array is array (natural range <>) of integer;
    constant def_arr : t_int_array;
end package;

package body deferred is
    constant def_arr : t_int_array := (0 to 2 => 10);
end package body;


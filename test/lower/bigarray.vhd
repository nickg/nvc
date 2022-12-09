package bigarray is
    type int_vec is array (natural range <>) of integer;
    function get_array return int_vec;
end package;

package body bigarray is
    function get_array return int_vec is
        variable r : int_vec(1 to 2**20) := (others => 2);
    begin
        return r;
    end function;
end package body;

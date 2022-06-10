package sumpkg is
    type int_vector is array (natural range <>) of integer;

    function get_left(a : int_vector) return integer;
    function get_right(a : int_vector) return integer;
    function get_length(a : int_vector) return integer;

    function sum(a : int_vector) return integer;
end package;

package body sumpkg is

    function sum(a : int_vector) return integer is
        variable result : integer := 0;
    begin
        for i in a'range loop
            result := result + a(i);
        end loop;
        return result;
    end function;

    function get_left(a : int_vector) return integer is
    begin
        return a'left;
    end function;

    function get_right(a : int_vector) return integer is
    begin
        return a'right;
    end function;

    function get_length(a : int_vector) return integer is
    begin
        return a'length;
    end function;
end package body;

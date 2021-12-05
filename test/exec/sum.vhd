package sumpkg is
end package;

package body sumpkg is

    type int_vector is array (natural range <>) of integer;

    function sum(a : int_vector) return integer is
        variable result : integer := 0;
    begin
        for i in a'range loop
            result := result + a(i);
        end loop;
        return result;
    end function;

end package body;

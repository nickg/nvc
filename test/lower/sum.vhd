package sumpkg is
    type int_vector is array (natural range <>) of integer;
    function sum(a : int_vector) return integer;
end package;

package body sumpkg is

    function sum(a : int_vector) return integer is
        variable result : integer := 0;
    begin
        sumloop: for i in a'range loop
            result := result + a(i);
        end loop;
        return result;
    end function;

end package body;

package tlab1 is
    function func1(n : integer) return integer;
end package;

package body tlab1 is
    type int_vector is array (natural range <>) of integer;

    function sum(a : int_vector) return integer is
        variable cnt : integer := 0;
    begin
        for i in a'range loop
            cnt := cnt + a(i);
        end loop;
        return cnt;
    end function;

    function iota(n : integer) return int_vector is
        variable r : int_vector(1 to n);
    begin
        for i in r'range loop
            r(i) := i;
        end loop;
        return r;
    end function;

    function func1(n : integer) return integer is
        constant c : int_vector := iota(n);
    begin
        return sum(c);
    end function;

end package body;

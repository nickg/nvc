package simple is
    procedure test_fact;
    procedure test_sum;
end package;

package body simple is

    function fact (n : integer) return integer is
        variable r : integer := 1;
    begin
        for i in 1 to n loop
            r := r * i;
        end loop;
        return r;
    end function;

    procedure test_fact is
        variable sum : integer;
    begin
        for i in 1 to 12 loop
            sum := sum + fact(i);
        end loop;
    end procedure;

    ---------------------------------------------------------------------------

    type int_vector is array (natural range <>) of integer;

    function sum(a : int_vector) return integer is
        variable result : integer := 0;
    begin
        for i in a'range loop
            result := result + a(i);
        end loop;
        return result;
    end function;

    procedure test_sum is
        variable dummy : integer;
        variable arr   : int_vector(1 to 1000);
    begin
        for i in 1 to arr'length loop
            arr(i) := i;
        end loop;
        for i in 1 to 100 loop
            dummy := dummy + sum(arr);
        end loop;
    end procedure;

end package body;

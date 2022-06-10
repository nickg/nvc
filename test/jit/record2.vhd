package pack3 is
    type int_vector is array (natural range <>) of integer;

    type rec is record
        x : integer;
        y : integer;
        a : int_vector(1 to 3);
        z : integer;
    end record;

    constant r : rec;
end package;

package body pack3 is
    constant r : rec := (1, 2, (3, 4, 5), 6);
end package body;

-------------------------------------------------------------------------------

package pack4 is
    function sum_fields return integer;
end package;

use work.pack3.all;

package body pack4 is
    function sum_fields return integer is
        variable sum : integer := r.x + r.y + r.z;
    begin
        --for i in r.a'range loop
        for i in 1 to 3 loop
            sum := sum + r.a(i);
        end loop;
        return sum;
    end function;
end package body;

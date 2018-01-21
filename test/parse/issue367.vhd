package r is

    function r1(a:bit_vector) return bit_vector;

end package;

package body r is

    function r1(a:bit_vector) return bit_vector is
        variable ret : bit_vector(a'range);
        variable i : integer range a'range;  -- Error here
    begin
        for i in a'range loop
            ret(i) := not a(i);
        end loop;

        return ret;
    end r1;

end r;

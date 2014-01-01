package elide is
end package;

package body elide is

    function func1(x : bit_vector) return bit_vector is
        alias a : bit_vector(x'length - 1 downto 0) is x;
        variable r : bit_vector(x'length - 1 downto 0);
    begin
        for i in r'range loop
            -- The bounds check for r(i) here should be optimised away
            r(i) := a(i);
        end loop;
        return r;
    end function;

end package body;

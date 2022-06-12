entity issue467 is
end entity;

architecture test of issue467 is

    type int_array is array (natural range <>) of integer_vector;

    function sum_all (x : int_array) return integer is
        variable result : integer := 0;
    begin
        for i in x'range loop
            for j in x(i)'range loop
                result := result + x(i)(j);
            end loop;
        end loop;
        return result;
    end function;

    function get_slice (x : int_array; l, r : natural) return int_array is
    begin
        return x(l to r);
    end function;

    signal s1 : int_array(1 to 3)(1 to 2) := ( (1, 2), (3, 4), (5, 6) );
begin

    p1: process is
    begin
        assert sum_all(s1) = 21;
        assert get_slice(s1, 1, 2) = ( (1, 2), (3, 4) );
        assert get_slice(s1, 3, 3) = ( 0 => (5, 6) );
        assert get_slice(s1, 3, 0) = ( 1 to 0 => (1, 1) );
        assert get_slice(s1, 1, 2)(2) = ( (3, 4) );

        s1(2)(2) <= 10;
        wait for 1 ns;
        assert sum_all(s1) = 27;

        wait;
    end process;

end architecture;

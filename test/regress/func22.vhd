entity func22 is
end entity;

architecture test of func22 is

    function iota return result_t of integer_vector is
        variable result : result_t;
        variable counter : natural;
    begin
        for i in result'range loop
            result(i) := counter;
            counter := counter + 1;
        end loop;
        return result;
    end function;

begin

    p1: process is
        variable v1 : integer_vector(1 to 3);
        variable v2 : integer_vector(7 to 12);
    begin
        v1 := iota;
        assert v1 = (0, 1, 2);
        v2 := iota;
        assert v2 = (0, 1, 2, 3, 4, 5);
        wait;
    end process;

end architecture;

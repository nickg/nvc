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

    signal s1 : integer_vector(1 to 5);
    signal s2 : integer_vector(6 to 6) := iota;

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

    p2: s2 <= iota;

    p3: process is
    begin
        assert s2 = (0 => 0);
        s1 <= iota;
        wait for 1 ns;
        assert s1 = (0, 1, 2, 3, 4);
        assert s2 = (0 => 0);
        wait;
    end process;

    p4: process is
        constant c1 : integer_vector(4 to 5) := iota;
    begin
        assert c1 = (0, 1);
        wait;
    end process;

end architecture;

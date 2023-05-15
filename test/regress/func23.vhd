entity func23 is
end entity;

architecture test of func23 is

    type rec_t is record
        f : integer_vector;
    end record;

    function iota return result_t of rec_t is
        variable result : result_t;
        variable counter : natural;
    begin
        for i in result.f'range loop
            result.f(i) := counter;
            counter := counter + 1;
        end loop;
        return result;
    end function;

    signal s1 : rec_t(f(1 to 5));
    signal s2 : rec_t(f(6 to 6)) := iota;

begin

    p1: process is
        variable v1 : rec_t(f(1 to 3));
        variable v2 : rec_t(f(7 to 12));
    begin
        v1 := iota;
        assert v1.f = (0, 1, 2);
        v2 := iota;
        assert v2.f = (0, 1, 2, 3, 4, 5);
        wait;
    end process;

    p2: s2 <= iota;

    p3: process is
    begin
        assert s2.f = (0 => 0);
        s1 <= iota;
        wait for 1 ns;
        assert s1.f = (0, 1, 2, 3, 4);
        assert s2.f = (0 => 0);
        wait;
    end process;

    p4: process is
        constant c1 : rec_t(f(4 to 5)) := iota;
    begin
        assert c1.f = (0, 1);
        wait;
    end process;

end architecture;

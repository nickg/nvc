entity array11 is
end entity;

architecture test of array11 is

    type rec is record
        x : integer_vector;
    end record;

    type rec_vec is array (natural range <>) of rec;

    function sum_all (r : rec_vec) return integer is
        variable result : integer := 0;
    begin
        for i in r'range loop
            for j in r(i).x'range loop
                result := result + r(i).x(j);
            end loop;
        end loop;
        return result;
    end function;

    signal s : rec_vec(1 to 2)(x(1 to 3));
begin

    p1: process is
    begin
        s <= ( ( x => (1, 2, 3) ),
               ( x => (4, 5, 6) ) );
        wait for 1 ns;
        assert sum_all(s) = 21;
        s(1).x(1) <= 5;
        wait for 1 ns;
        assert sum_all(s) = 25;
        wait;
    end process;

end architecture;

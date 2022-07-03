entity array12 is
end entity;

architecture test of array12 is

    type rec is record
        x : integer_vector;
    end record;

    type rec_vec is array (natural range <>) of rec;
    type rec_vec_vec is array (natural range <>) of rec_vec;

    function sum_all (r : rec_vec_vec) return integer is
        variable result : integer := 0;
    begin
        for i in r'range loop
            for j in r(i)'range loop
                for k in r(i)(j).x'range loop
                    result := result + r(i)(j).x(k);
                end loop;
            end loop;
        end loop;
        return result;
    end function;

    signal s : rec_vec_vec(1 to 2)(1 to 1)(x(1 to 3));
begin

    p1: process is
    begin
        s <= ( ( 1 => ( x => (1, 2, 3) ) ),
               ( 1 => ( x => (4, 5, 6) ) ) );
        wait for 1 ns;
        assert sum_all(s) = 21;
        s(1)(1).x(1) <= 5;
        wait for 1 ns;
        assert sum_all(s) = 25;
        wait;
    end process;

end architecture;

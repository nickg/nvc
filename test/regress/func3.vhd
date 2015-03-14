entity func3 is
end entity;

architecture test of func3 is

    type int_array is array (integer range <>) of integer;

    function copy_and_sum_1(x : int_array) return integer is
        variable tmp : int_array(1 to x'length);
        variable sum : integer := 0;
    begin
        for i in x'range loop
            tmp(i) := x(i);
        end loop;
        for i in tmp'range loop
            sum := sum + tmp(i);
        end loop;
        return sum;
    end function;

    function copy_and_sum_2(x : int_array) return integer is
        variable tmp : int_array(x'range);
        variable sum : integer := 0;
    begin
        for i in x'range loop
            tmp(i) := x(i);
        end loop;
        for i in tmp'range loop
            sum := sum + tmp(i);
        end loop;
        return sum;
    end function;

    function copy_and_add_1(x : int_array; y : integer) return int_array is
        variable tmp : int_array(1 to 5);
    begin
        for i in 1 to 5 loop
            tmp(i) := x(i) + y;
        end loop;
        return tmp;
    end function;

    function copy_and_add_2(x : int_array; y : integer) return int_array is
        variable tmp : int_array(x'range);
    begin
        for i in tmp'range loop
            tmp(i) := x(i) + y;
            report integer'image(tmp(i));
        end loop;
        return tmp;
    end function;

begin

    process is
        variable a : int_array(1 to 5) := (1, 2, 3, 4, 5);
        variable b : int_array(6 downto 2) := (1, 2, 3, 4, 5);
        variable c : int_array(1 to 5);
    begin
        assert copy_and_sum_1(a) = 15;
        assert copy_and_sum_2(a) = 15;
        assert copy_and_sum_2(b) = 15;
        c := copy_and_add_1(a, 1);
        assert c = (2, 3, 4, 5, 6);
        c := copy_and_add_2(a, 5);
        assert c = (6, 7, 8, 9, 10);
        wait;
    end process;

end architecture;

entity assign8 is
end entity;

architecture test of assign8 is

    function get_ints(count : natural) return integer_vector is
        variable result : integer_vector(1 to count);
    begin
        for i in 1 to count loop
            result(i) := i;
        end loop;
        return result;
    end function;

    procedure proc (xl, yl : natural) is
        variable x : integer_vector(1 to xl);
        variable y : integer_vector(1 to yl);
    begin
        (x, y) := get_ints(5);
        for i in 1 to xl loop
            assert x(i) = i;
        end loop;
        for i in 1 to yl loop
            assert y(i) = xl + i;
        end loop;
    end procedure;
begin

    p1: process is
    begin
        proc(2, 3);
        proc(1, 4);
        proc(0, 5);
        proc(5, 0);
        proc(4, 1);
        wait;
    end process;

end architecture;

entity integer2 is
end entity;

architecture test of integer2 is
    type array_t is array (integer range <>) of integer;

    function get_length (a : array_t) return integer is
    begin
        return a'length;
    end function;

    function get_array return array_t is
    begin
        return (1, 2, 3);
    end function;
begin

    p1: process is
        variable x, y : integer;
    begin
        assert x = -9223372036854775807 - 1;
        x := 1;
        y := 5;
        wait for 1 ns;
        assert x + y = 6;
        x := integer(integer'high) + 1;
        report integer'image(x);
        assert integer'image(x) = "-9223372036854775808";
        assert integer'value("-9223372036854775807") = -9223372036854775807;
        wait;
    end process;

    p2: process is
        variable a : array_t(-9223372036854775807 to -9223372036854775800);
        constant c : array_t := get_array;
    begin
        assert a'length = 8;
        assert get_length(a) = 8;
        assert c'length = 3;
        report integer'image(a'left);
        wait for 1 ns;
        assert c'left = integer'left;
        assert c'right = integer'left + 2;
        assert c(-9223372036854775807) = 2;
        wait;
    end process;

end architecture;

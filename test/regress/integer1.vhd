entity integer1 is
end entity;

architecture test of integer1 is
    type int64_t is range -9223372036854775807 - 1 to 9223372036854775807;
    type array_t is array (int64_t range <>) of int64_t;

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
        variable x, y : int64_t;
    begin
        assert x = -9223372036854775807 - 1;
        x := 1;
        y := 5;
        wait for 1 ns;
        assert x + y = 6;
        x := int64_t(integer'high) + 1;
        report int64_t'image(x);
        assert int64_t'image(x) = "2147483648";
        assert int64_t'value("-9223372036854775807") = -9223372036854775807;
        wait;
    end process;

    p2: process is
        variable a : array_t(-9223372036854775807 to -9223372036854775800);
        constant c : array_t := get_array;
    begin
        assert a'length = 8;
        assert get_length(a) = 8;
        assert c'length = 3;
        report int64_t'image(a'left);
        wait for 1 ns;
        assert c'left = int64_t'left;
        assert c'right = int64_t'left + 2;
        assert c(-9223372036854775807) = 2;
        wait;
    end process;

end architecture;

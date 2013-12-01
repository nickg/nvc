entity operator5 is
end entity;

architecture test of operator5 is

    type int_vec2 is array (integer range <>) of integer;
    type int_vec is array (integer range <>) of integer;

    function ">="(a, b : int_vec) return boolean is
    begin
        return false;
    end function;

begin

    process is
        variable x, y : int_vec(1 to 3);
    begin
        x := (1, 2, 3);
        y := (4, 5, 6);
        assert not (y >= x);
        assert (int_vec2(y) >= int_vec2(x));
        wait;
    end process;

end architecture;

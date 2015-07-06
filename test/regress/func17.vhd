entity func17 is
end entity;

architecture test of func17 is

    function func(x : bit_vector) return bit_vector is
        variable y : bit_vector(1 to x'length) := x;
    begin
        y(1 + x'length / 2) := '1';     -- Would corrupt X
        return y;
    end function;

begin

    process is
        variable b : bit_vector(1 to 3);
    begin
        b := "101";
        assert func(b) = "111";
        assert b = "101";
        wait;
    end process;

end architecture;

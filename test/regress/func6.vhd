entity func6 is
end entity;

architecture test of func6 is

    function flip(x : bit_vector(3 downto 0)) return bit_vector is
        variable r : bit_vector(3 downto 0);
    begin
        r(0) := x(3);
        r(1) := x(2);
        r(2) := x(1);
        r(3) := x(0);
        return r;
    end function;

    function flipu(x : bit_vector) return bit_vector is
    begin
        return flip(x);
    end function;

    function flipu2(x : bit_vector) return bit_vector is
    begin
        return flip(x(3 downto 0));
    end function;
begin

    process is
        variable b : bit_vector(3 downto 0);
    begin
        assert flip("1010") = "0101";
        b := "1100";
        assert flip(b) = "0011";
        assert flipu(b) = "0011";
        assert flipu2(b) = "0011";
        wait;
    end process;

end architecture;

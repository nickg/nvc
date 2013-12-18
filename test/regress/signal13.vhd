entity signal13 is
end entity;

architecture test of signal13 is

    function invert(signal x : in bit) return bit is
    begin
        return not x;
    end function;

    signal vec : bit_vector(1 to 3) := "101";
begin

    process is
    begin
        assert invert(vec(1)) = '0';
        assert invert(vec(2 to 3)(2)) = '1';
        wait;
    end process;

end architecture;

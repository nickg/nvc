entity bitvec is
end entity;

architecture test of bitvec is

    function get_bitvec(x, y : integer) return bit_vector is
        variable r : bit_vector(x to y) := "00";
    begin
        return r;
    end function;

begin

    process is
        variable b : bit_vector(3 downto 0);
        variable n : integer;
    begin
        b := "1101";
        n := 2;
        wait for 1 ns;
        assert not b = "0010";
        assert (b and "1010") = "1000";
        assert (b or "0110") = "1111";
        assert (b xor "0111") = "1010";
        assert (b xnor "0111") = "0101";
        assert (b nand "1010") = "0111";
        assert (b nor "0110") = "0000";
        assert get_bitvec(1, n) = "00";
        wait;
    end process;

end architecture;

entity const7 is
end entity;

architecture test of const7 is

    function get_bits(x, len : natural) return bit_vector is
    begin
        if x = 1 then
            return bit_vector'(1 to len => '1');
        else
            return bit_vector'(1 to len => '0');
        end if;
    end function;

    signal x : bit_vector(7 downto 0) := get_bits(1, 8);
    signal y : bit_vector(3 downto 0) := get_bits(5, 4);

begin

    process is
    begin
        assert x = X"ff";
        assert y = X"0";
        wait;
    end process;

end architecture;

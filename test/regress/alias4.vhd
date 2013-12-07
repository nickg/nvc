entity alias4 is
end entity;

architecture test of alias4 is

    function func(x : bit_vector(7 downto 0); k : bit_vector) return bit is
        alias y : bit_vector(k'range) is x;
    begin
        return y(1);
    end function;

begin

    process is
        variable x : bit_vector(7 downto 0);
    begin
        x := X"ab";
        assert func(x, x) = '1';
        wait;
    end process;

end architecture;

entity hintbug is
end entity;

architecture test of hintbug is
    function func(x : bit) return bit_vector is
    begin
        return x & '1';
    end function;
begin

    p1: process is
        variable v : bit_vector(1 downto 0);
        variable x : bit := '1';
    begin
        v := func(x);                   -- Will create an unused storage hint
        assert v = x & '0';             -- Will incorrectly use above hint

        wait;
    end process;

end architecture;

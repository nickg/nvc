entity wait17 is
end entity;

architecture test of wait17 is

    function func (x : bit) return bit_vector is
    begin
        return (0 to 7 => x);
    end function;

    signal result, x : bit;

begin

    p1: result <= func(x)(0);

    p2: process is
    begin
        assert result = '0';
        x <= '1';
        wait for 1 ns;
        assert result = '1';
        wait;
    end process;

end architecture;

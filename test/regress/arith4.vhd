entity arith4 is
end entity;

architecture test of arith4 is
    type my_uint16 is range 0 to (2 ** 16) - 1;
begin

    p1: process is
        variable x, y : my_uint16 := 0;
    begin
        assert x ** 5 = 0;
        x := 2;
        wait for 1 ns;
        assert x ** 9 = 512;            -- OK
        assert x ** 15 = 32768;         -- OK
        y := x ** 16;                   -- Error
        wait;
    end process;

end architecture;

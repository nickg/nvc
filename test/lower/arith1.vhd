entity arith1 is
end entity;

architecture test of arith1 is
begin

    p1: process is
        variable x, y : integer;
    begin
        x := 3;
        y := 12;
        wait for 1 ns;
        assert x + y = 15;
        assert x - y = -9;
        assert x * y = 36;
        assert x / 12 = 0;
        assert x = 3;
        assert y = 12;
        assert x /= y;
        assert x < y;
        assert y > x;
        assert x <= y;
        assert y >= x;
        assert (- x) = -3;
        assert x ** y = 531441;
        x := -34;
        assert abs x = 34;
        assert abs y = 12;
        assert 5 mod x = 2;
        assert 5 rem x = 2;
        assert (-5) rem x = -2;
        assert (-5) mod x = 2;
        assert x = +x;
        assert 0 - x > 0;
        assert x ** 0 = 1;
        assert x ** 1 = x;
        wait;
    end process;

end architecture;

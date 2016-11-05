entity arith1 is
end entity;

architecture test of arith1 is
begin

    proc1: process is
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
        y := 3;
        assert 5 mod y = 2;
        assert 5 rem y = 2;
        assert (-5) rem y = -2;
        assert (-5) mod y = 2;
        assert x = +x;
        wait;
    end process;

end architecture;

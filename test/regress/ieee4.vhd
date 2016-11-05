entity ieee4 is
end entity;

library ieee;
use ieee.math_real.all;

architecture test of ieee4 is

    function approx(x, y : real; t : real := 0.001) return boolean is
    begin
        return abs(x - y) < t;
    end function;

begin

    process is
        variable s1, s2 : integer;
        variable r : real;
    begin
        r := 6.8;
        wait for 0 ns;                  -- Prevent constant folding
        assert approx(sign(r), 1.0);
        r := 5.7;
        wait for 0 ns;
        assert approx(ceil(r), 6.0);
        r := 0.6;
        wait for 0 ns;
        assert approx(floor(r), 0.0);
        r := 0.5;
        wait for 0 ns;
        assert approx(round(r), 1.0);
        r := 6.4999;
        wait for 0 ns;
        assert approx(round(r), 6.0);
        r := 0.999;
        wait for 0 ns;
        assert approx(trunc(r), 0.0);
        r := 4.6;
        wait for 0 ns;
        assert approx(r mod 2.7, 1.9);

        s1 := 6;
        s2 := 883;
        uniform(s1, s2, r);
        assert approx(r, 0.983380);
        uniform(s1, s2, r);
        assert approx(r, 0.627369);
        uniform(s1, s2, r);
        assert approx(r, 0.883711);
        uniform(s1, s2, r);
        assert approx(r, 0.472620);
        uniform(s1, s2, r);
        assert approx(r, 0.582179);

        r := 4.0;
        wait for 0 ns;
        assert approx(sqrt(r), 2.0);
        r := 4.3;
        wait for 0 ns;
        assert approx(sqrt(r), 2.0736);
        r := 612.8;
        wait for 0 ns;
        assert approx(cbrt(r), 8.49388);
        r := 1.2;
        wait for 0 ns;
        assert approx(5 ** r, 6.8986);
        r := 2.0;
        wait for 0 ns;
        assert approx(r ** (-1.0), 0.5);
        r := 2.0;
        wait for 0 ns;
        assert approx(exp(r), 7.389056);
        r := 1.0;
        wait for 0 ns;
        assert approx(log(r), 0.0);
        r := MATH_E;
        wait for 0 ns;
        assert approx(log(r), 1.0);
        r := 5216.72;
        wait for 0 ns;
        assert approx(log(r), 8.5596);
        r := MATH_PI;
        wait for 0 ns;
        assert approx(sin(r), 0.0);
        r := 1.15251;
        wait for 0 ns;
        assert approx(cos(r), 0.406195);
        r := 0.5;
        wait for 0 ns;
        assert approx(arctan(r), 0.463648);

        wait;
    end process;

end architecture;

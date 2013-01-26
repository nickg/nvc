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
        assert approx(sign(6.8), 1.0);
        assert approx(ceil(5.7), 6.0);
        assert approx(floor(0.6), 0.0);
        assert approx(round(0.5), 1.0);
        assert approx(round(6.4999), 6.0);
        assert approx(trunc(0.999), 0.0);
        assert approx(4.6 mod 2.7, 1.9);

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

        --report real'image(sqrt(4.0));
        --assert approx(sqrt(4.0), 2.0);

        assert approx(sin(MATH_PI), 0.0);

        wait;
    end process;

end architecture;

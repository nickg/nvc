entity should_fold is
    generic ( X : boolean );
end entity;

architecture test of should_fold is
begin
    g: if not X generate                -- Error if not constant folded
        assert X;
    end generate;
end architecture;

-------------------------------------------------------------------------------

entity ieee5 is
end entity;

library ieee;
use ieee.math_real.all;

architecture test of ieee5 is
    function approx(x, y : real; t : real := 0.001) return boolean is
    begin
        return abs(x - y) < t;
    end function;
begin

    s1: entity work.should_fold
        generic map ( approx(sign(6.8), 1.0) );
    s2: entity work.should_fold
        generic map ( approx(ceil(5.7), 6.0) );
    s3: entity work.should_fold
        generic map ( approx(floor(0.6), 0.0) );
    s4: entity work.should_fold
        generic map ( approx(round(0.5), 1.0) );
    s5: entity work.should_fold
        generic map ( approx(round(6.4999), 6.0) );
    s6: entity work.should_fold
        generic map ( approx(trunc(0.999), 0.0) );
    s7: entity work.should_fold
        generic map ( approx(4.6 mod 2.7, 1.9) );
    s8: entity work.should_fold
        generic map ( approx(sin(MATH_PI), 0.0) );
    s9: entity work.should_fold
        generic map ( approx(cos(1.15251), 0.406195) );
    s10: entity work.should_fold
        generic map ( approx(arctan(0.5), 0.463648) );

end architecture;

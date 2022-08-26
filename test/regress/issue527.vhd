library ieee;
use ieee.math_real.all;

entity issue527 is
end entity;

architecture test of issue527 is
    signal s : bit;
begin

    p1: process (s) is
        variable s1, s2 : positive := 12345;

        impure function random (x, y : delay_length) return delay_length is
            variable r : real;
        begin
            uniform(s1, s2, r);
            return x + r * (y - x);
        end function;

    begin
        s <= not s after random(1 ns, 2 ns);
    end process;

end architecture;

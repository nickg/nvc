package math_real_perf is
    function test_sin return real;
    function test_cos return real;
end package;

library ieee;
use ieee.math_real.all;

package body math_real_perf is

    function test_sin return real is
        variable r, accum : real := 0.0;
    begin
        while r < 100.0 loop
            accum := accum + sin(r);
            r := r + 0.1;
        end loop;
        return r;
    end function;

    function test_cos return real is
        variable r, accum : real := 0.0;
    begin
        while r < 100.0 loop
            accum := accum + cos(r);
            r := r + 0.1;
        end loop;
        return r;
    end function;

end package body;

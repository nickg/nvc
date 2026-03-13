-- Package compiled into other_lib in the shell driver.  Although its
-- name matches the +package *_pkg pattern, the -hierarchy other_lib.*
-- rule in cover30.spec excludes it from the coverage report.
package util_pkg is
    function clamp(x, lo, hi : integer) return integer;
end package;

package body util_pkg is

    function clamp(x, lo, hi : integer) return integer is
    begin
        if x < lo then
            return lo;
        elsif x > hi then
            return hi;
        else
            return x;
        end if;
    end function;

end package body;

-------------------------------------------------------------------------------

-- Package compiled into the work library: should be covered
-- (+package *_pkg).
package math_pkg is
    function abs_diff(a, b : integer) return integer;
end package;

package body math_pkg is

    function abs_diff(a, b : integer) return integer is
    begin
        if a >= b then
            return a - b;
        else
            return b - a;
        end if;
    end function;

end package body;

-------------------------------------------------------------------------------

library other_lib;
use other_lib.util_pkg.all;

use work.math_pkg.all;

entity cover30 is
end cover30;

architecture test of cover30 is
begin
    process
        variable v : integer;
    begin
        v := abs_diff(10, 3);
        v := clamp(v, 0, 5);
        assert v = 5;
        wait;
    end process;
end architecture;

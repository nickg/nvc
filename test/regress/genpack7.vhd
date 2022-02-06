package poly is
    generic (a, b, def : integer);
    function apply (x : integer := def) return integer;
end package;

package body poly is
    function apply (x : integer := def) return integer is
    begin
        return x * a + b;
    end function;
end package body;

-------------------------------------------------------------------------------

package wrapper is
    generic ( package p is new work.poly generic map ( <> ) );
    function wrapped_apply (n : integer) return integer;
end package;

package body wrapper is
    use p.all;

    function wrapped_apply (n : integer) return integer is
    begin
        return apply;
    end function;
end package body;

-------------------------------------------------------------------------------

package my_poly1 is new work.poly generic map (a => 2, b => 3, def => 10);
package my_wrap1 is new work.wrapper generic map (p => work.my_poly1);

package my_poly2 is new work.poly generic map (a => 5, b => 1, def => 1);
package my_wrap2 is new work.wrapper generic map (p => work.my_poly2);

-------------------------------------------------------------------------------

entity genpack7 is
end entity;

use work.my_wrap1;
use work.my_wrap2;

architecture test of genpack7 is
begin

    main: process is
        variable v : integer := 5;
    begin
        assert my_wrap1.wrapped_apply(2) = 23;
        wait for 1 ns;
        assert my_wrap1.wrapped_apply(v) = 23;

        assert my_wrap2.wrapped_apply(2) = 6;
        assert my_wrap2.wrapped_apply(v) = 6;
        wait;
    end process;

end architecture;

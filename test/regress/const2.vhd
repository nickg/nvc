package pack is

    function foo(x : in integer) return real;

end package;

package body pack is

    type real_vector is array (integer range <>) of real;

    function get_results return real_vector is
    begin
        return ( 52.6, 16.7, 1.832, 0.623, 762.236 );
    end function;

    constant results : real_vector := get_results;

    function foo(x : in integer) return real is
    begin
        return results(x);
    end function;

end package body;

-------------------------------------------------------------------------------

use work.pack.all;

entity const2 is
end entity;

architecture test of const2 is

    function get_it return integer is
    begin
        return integer(foo(integer'left + 1));
    end function;

begin

    process is
    begin
        assert get_it = 16;
        wait;
    end process;

end architecture;

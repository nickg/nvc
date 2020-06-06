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

    type int_vector is array (integer range <>) of integer;

    subtype int_vector4 is int_vector(1 to 4);

    constant blah  : int_vector4 := ( 0, 1, 6, 6 );
    constant blah2 : int_vector4 := blah;

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

    function get_bits return bit_vector is
    begin
        return "110101";
    end function;

    constant some_bits : bit_vector := get_bits;

    constant a_bit : bit := some_bits(2);

begin

    process is
    begin
        assert get_it = 17;
        assert some_bits(some_bits'right) = '1';
        assert a_bit = '0';
        wait;
    end process;

end architecture;

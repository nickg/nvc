package generic_complex is generic (
    type t ;

    -- default subprograms
    function "+"(l, r : t) return t is <> ;
    function to_string(x : t) return string is <>
) ;

    -- cute way of making an array of type t with indices for re and im parts
    type complex_parts is (re, im) ;
    type complex is array(complex_parts) of t ;

    -- complex summing operation
    function "+"(l, r : complex) return complex ;

    -- string representation
    function to_string(x : complex) return string ;

end package ;

package body generic_complex is

    function "+"(l, r : complex) return complex is
    begin
        return (re => l(re)+r(re), im => l(im)+r(im)) ;
    end function ;

    function to_string(x : complex) return string is
    begin
        return "(" & to_string(x(re)) & "," & to_string(x(im)) & ")" ;
    end function ;

end package body ;

entity pipelined_complex_sum is
  generic (
    package complex_pkg is new work.generic_complex generic map(<>)
  ) ;
  port (
    clock   :   in  bit ;
    a       :   in  complex_pkg.complex ;
    b       :   in  complex_pkg.complex ;
    c       :   out complex_pkg.complex
  ) ;
end entity ;

architecture arch of pipelined_complex_sum is

    use complex_pkg.all ;

begin

    process(clock)
    begin
        if( rising_edge(clock) ) then
            c <= a + b ;
        end if ;
    end process ;

end architecture ;

-- create the two different complex packages
package complex_int  is new work.generic_complex generic map (t => integer) ;
package complex_real is new work.generic_complex generic map (t => real) ;

use work.complex_real.all ;
use work.complex_int.all ;

entity issue881 is
end entity ;

architecture arch of issue881 is

    -- signals for real summer
    signal ar : work.complex_real.complex ;
    signal br : work.complex_real.complex ;
    signal cr : work.complex_real.complex ;

    -- signals for int summer
    signal ai : work.complex_int.complex ;
    signal bi : work.complex_int.complex ;
    signal ci : work.complex_int.complex ;

    -- clock
    signal clock : bit ;

begin

    clock <= not clock after 1 ns ;

    U_real_summer : entity work.pipelined_complex_sum
      generic map (
        complex_pkg => work.complex_real
      ) port map (
        clock   => clock,
        a       => ar,
        b       => br,
        c       => cr
      ) ;

    U_int_summer : entity work.pipelined_complex_sum
      generic map (
        complex_pkg => work.complex_int
      ) port map (
        clock   => clock,
        a       => ai,
        b       => bi,
        c       => ci
      ) ;

    tb : process
        variable val : work.complex_int.complex ;
    begin
        for i in 1 to 10 loop
            val := (i, 42*i) ;
            ai <= val ;
            ar <= (real(val(re)), real(val(im))) ;

            val := (-7*i, 100*i) ;
            bi <= val ;
            br <= (real(val(re)), real(val(im))) ;

            wait until rising_edge(clock) ;
        end loop ;

        assert ci = (-54, 1278);
        std.env.stop ;
    end process ;

end architecture ;

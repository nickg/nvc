-- Test case from Brian Padalino
library ieee ;
    use ieee.std_logic_1164.all ;
    use ieee.numeric_std.all ;
    use ieee.math_real.all ;

    use std.textio.all ;

entity issue615 is
end entity ;

architecture arch of issue615 is

    constant LUT_DEPTH : positive := 32 ;

    type cx_t is record
        re  :   signed ;
        im  :   signed ;
    end record ;

    function to_string(x : cx_t) return string is
    begin
        return "(" &
               to_string(to_integer(x.re)) &
               "," &
               to_string(to_integer(x.im)) &
               ")" ;
    end function ;

    subtype c18_t is cx_t( re(17 downto 0), im(17 downto 0) );

    type c18s_t is array(natural range <>) of c18_t ;

    function sincos(n : positive) return c18s_t is
        variable rv : c18s_t(0 to n-1) ;
    begin
        for idx in 0 to n-1 loop
            rv(idx).re := to_signed(integer(round(32768.0*cos(2.0*MATH_PI*real(idx)/real(n)))), 18) ;
            rv(idX).im := to_signed(integer(round(32768.0*sin(2.0*MATH_PI*real(idx)/real(n)))), 18) ;
        end loop ;
        return rv ;
    end function ;

    constant sincos_lut : c18s_t(0 to LUT_DEPTH-1) := sincos(LUT_DEPTH) ;

begin

    tb : process
        variable l : line ;
    begin
        for idx in sincos_lut'range loop
            write(l, to_string(sincos_lut(idx))) ;
            writeline(output, l) ;
        end loop ;
        std.env.stop ;
    end process ;

end architecture ;

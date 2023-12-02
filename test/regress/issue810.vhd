library ieee ;
use ieee.fixed_pkg.all ;

package types is

    type sfixed_vector is array(natural range <>) of sfixed ;

end package ;
library ieee ;
use ieee.numeric_std.all ;
use ieee.math_real.all ;
use ieee.fixed_pkg.all ;
use ieee.float_pkg.all ;

package convert is

    function from_real(x : in real ; size : in real) return real ;
    function from_real generic(scale : real := 65536.0) parameter (x : in real ; size : in integer) return integer ;
    function from_real generic(scale : real := 65536.0) parameter (x : in real ; size : in signed) return signed ;
    function from_real(x : in real ; size : in sfixed) return sfixed ;
    function from_real(x : in real ; size : in float) return float ;

    function resize(x : in real ; size : in real) return real ;
    function resize(x : in sfixed ; size : in sfixed) return sfixed ;
    function resize(x : in float ; size : in float) return float ;

    function to_real(x : in real) return real ;
    function to_real generic(scale : real := 65536.0) parameter (x : in integer) return real ;
    function to_real generic(scale : real := 65536.0) parameter (x : in signed) return real ;
    function to_real(x : in float) return real ;

    function shift_right(x : in real ; n : in natural) return real ;
    function shift_right(x : in integer ; n : in natural) return integer ;
    function shift_right(x : in float ; n : in natural) return float ;

end package ;

package body convert is

    function shift_right(x : in float ; n : in natural) return float is
    begin
        return x / (2.0**n) ;
    end function ;

    function shift_right(x : in real ; n : in natural) return real is
    begin
        return x / (2.0**n) ;
    end function ;

    function shift_right(x : in integer ; n : in natural) return integer is
    begin
        return x / integer(2.0**n) ;
    end function ;

    function to_real generic (scale : real := 65536.0 ) parameter (x : in signed) return real is
    begin
        return real(to_integer(x))/scale ;
    end function ;

    function to_real(x : in float) return real is
    begin
        return ieee.float_pkg.to_real(x) ;
    end function ;

    function to_real(x : in real) return real is
    begin
        return x ;
    end function ;

    function to_real generic (scale : real := 65536.0) parameter (x : in integer) return real is
    begin
        return real(x)/scale ;
    end function ;

    function resize(x : in float ; size : in float) return float is
    begin
        return ieee.float_pkg.resize(x, size) ;
    end function ;

    function resize(x : in sfixed ; size : in sfixed) return sfixed is
    begin
        return ieee.fixed_pkg.resize(x, size) ;
    end function ;

    function resize(x : in real ; size : in real) return real is
    begin
        return x ;
    end function ;

    function from_real(x : in real ; size : in float) return float is
    begin
        return to_float(x, size) ;
    end function ;

    function from_real(x : in real ; size : in real) return real is
    begin
        return x ;
    end function ;

    function from_real generic(scale : real := 65536.0) parameter (x : in real ; size : in integer) return integer is
    begin
        return integer(round(scale*x)) ;
    end function ;

    function from_real generic(scale : real := 65536.0) parameter (x : in real ; size : in signed) return signed is
    begin
        return to_signed(integer(scale*x), size'length) ;
    end function ;

    function from_real(x : in real ; size : in sfixed) return sfixed is
    begin
        return to_sfixed(x, size) ;
    end function ;

end package body ;

library work ;
use work.convert.to_real ;
use work.convert.from_real ;

package convert_integer is
    function from_real_q12 is new from_real[real, integer return integer] generic map (scale => 4096.0) ;
    function from_real_q15 is new from_real[real, integer return integer] generic map (scale => 32768.0) ;

    function to_real_q12 is new to_real[integer return real] generic map (scale => 4096.0) ;
    function to_real_q15 is new to_real[integer return real] generic map (scale => 32768.0) ;

    function resize(x : in integer ; size : in integer) return integer ;
end package ;

package body convert_integer is
    function resize(x : in integer ; size : in integer) return integer is
    begin
        return x ;
    end function ;
end package body ;

library ieee ;
library work ;
use ieee.numeric_std.all ;
use work.convert.to_real ;
use work.convert.from_real ;

package convert_signed is
    function from_real_sc12 is new from_real[real, signed return signed] generic map (scale => 4096.0) ;
    function from_real_sc15 is new from_real[real, signed return signed] generic map (scale => 32768.0) ;

    function to_real_sc12 is new to_real[signed return real] generic map (scale => 4096.0) ;
    function to_real_sc15 is new to_real[signed return real] generic map (scale => 32768.0) ;
end package ;

package generic_complex_pkg is
  generic (
    type T ;
    function "+"(l, r : T) return T is <> ;
    function "-"(l, r : T) return T is <> ;
    function "*"(l, r : T) return T is <> ;
    function "/"(l, r : T) return T is <> ;
    function to_real(x : in T) return real is <> ;
    function from_real(x : in real ; size : in T) return T is <> ;
    function to_string(arg : T) return string is <> ;
    function resize(arg : in T ; size : in T) return T is <> ;
    function shift_right(arg : in T ; n : in natural) return T is <>
  ) ;

    constant COPYRIGHT : string := "Copyright Brian Padalino (bpadalino@gmail.com).  Licensed Apache 2.0." ;

    type complex is record
        re : T ;
        im : T ;
    end record ;

    type complex_vector is array(natural range <>) of complex ;

    function "+"(l, r : in complex) return complex ;
    function "+"(l : in complex ; r : in T) return complex ;
    function "+"(l : in T ; r : in complex) return complex ;

    function "-"(l, r : in complex) return complex ;
    function "-"(l : in complex ; r : in T) return complex ;
    function "-"(l : in T ; r : in complex) return complex ;

    function "*"(l, r : in complex) return complex ;
    function "*"(l : in complex ; r : in T) return complex ;
    function "*"(l : in T ; r : in complex) return complex ;

    function "/"(l, r : in complex) return complex ;
    function "/"(l : in complex ; r : in T) return complex ;
    function "/"(l : in T ; r : in complex) return complex ;

    function conj(z : in complex) return complex ;

    function magsq(z : in complex) return T ;

    function to_string(arg : complex) return string ;

    function to_complex(x : in real ; y : in real := 0.0 ; size : complex) return complex ;
    function to_complex(x : in real ; y : in real := 0.0 ; size : T) return complex ;

    function resize(arg : in complex ; size : in complex) return complex ;

    function shift_right(arg : in complex ; n : in natural) return complex ;

end package ;

package body generic_complex_pkg is

    function magsq(z : in complex) return T is
    begin
        return z.re*z.re + z.im*z.im ;
    end function ;

    function shift_right(arg : in complex ; n : in natural) return complex is
    begin
        return complex'(shift_right(arg.re, n), shift_right(arg.im, n)) ;
    end function ;

    function "+"(l, r : in complex) return complex is
    begin
        return complex'(re => (l.re + r.re), im => (l.im + r.im)) ;
    end function ;

    function "+"(l : in complex ; r : in T) return complex is
    begin
        return l + complex'(re => r, im => from_real(0.0, r)) ;
    end function ;

    function "+"(l : in T ; r : in complex) return complex is
    begin
        return r + complex'(re => l, im => from_real(0.0, l)) ;
    end function ;


    function "-"(l, r : in complex) return complex is
    begin
        return complex'(re => (l.re - r.re), im => (l.im - r.im)) ;
    end function ;

    function "-"(l : in complex ; r : in T) return complex is
    begin
        return l - complex'(re => r, im => from_real(0.0, r)) ;
    end function ;

    function "-"(l : in T ; r : in complex) return complex is
    begin
        return complex'(re => l, im => from_real(0.0, l)) - r ;
    end function ;


    function "*"(l, r : in complex) return complex is
    begin
        return complex'(re => (l.re*r.re - l.im*r.im), im => (l.re*r.im + l.im*r.re)) ;
    end function ;

    function "*"(l : in complex ; r : in T) return complex is
    begin
        return l * complex'(re => r, im => from_real(0.0, r)) ;
    end function ;

    function "*"(l : in T ; r : in complex) return complex is
    begin
        return complex'(re => l, im => from_real(0.0, l)) * r ;
    end function ;


    function "/"(l, r : in complex) return complex is
        constant temp : T := magsq(r) ;
    begin
        if temp = from_real(0.0, temp) then
            report "Attempt to divide complex by (0.0, 0.0)" severity error ;
            return complex'(from_real(real'high, temp), from_real(0.0, temp)) ;
        end if ;

        return complex'( re => (l.re*r.re + l.im*r.im) / temp, im => (l.im*r.re - l.re*r.im)/temp) ;
    end function ;

    function "/"(l : in complex ; r : in T) return complex is
    begin
        if r = from_real(0.0, r) then
            report "Attempt to divide complex by 0.0" severity error ;
            return complex'(re => from_real(real'high, r), im => from_real(0.0, r)) ;
        end if ;
        return complex'(l.re / r, l.im / r) ;
    end function ;

    function "/"(l : in T ; r : in complex) return complex is
    begin
        if r.re = from_real(0.0, r.re) and r.im = from_real(0.0, r.im) then
            report "Attempt to divide complex by (0.0, 0.0)" severity error ;
            return complex'(re => from_real(real'high, r.re), im => from_real(0.0, r.im)) ;
        end if ;
        return complex'(l/magsq(r) * r.re, (from_real(0.0, r.im)-(l/magsq(r))) * r.im) ;
    end function ;


    function conj(z : in complex) return complex is
    begin
        return complex'(re => z.re, im => from_real(0.0, z.im)-z.im) ;
    end function ;

    function to_string(arg : complex) return string is
    begin
        return "(" & to_string(to_real(arg.re)) & "," & to_string(to_real(arg.im)) & "j)" ;
    end function ;

    function to_complex(x : in real ; y : in real := 0.0; size : complex) return complex is
    begin
        return complex'(re => from_real(x, size.re), im => from_real(y, size.im)) ;
    end function ;

    function to_complex(x : in real ; y : in real := 0.0; size : T) return complex is
    begin
        return complex'(re => from_real(x, size), im => from_real(y, size)) ;
    end function ;

    function resize(arg : in complex ; size : in complex) return complex is
    begin
        return complex'(re => resize(arg.re, size.re), im => resize(arg.im, size.im)) ;
    end function ;

end package body ;

library ieee ;
library work ;
use ieee.fixed_pkg.all ;
use work.convert.all ;
package complex_sfixed is new work.generic_complex_pkg generic map(T => sfixed, resize => work.convert.resize) ;

library ieee ;
use ieee.std_logic_1164.all ;
use ieee.math_real.all ;
use ieee.fixed_pkg.all ;
use work.complex_sfixed.all ;
use work.types.all ;

entity pfb_analysis is
  generic (
    H           :   sfixed_vector ;
    N           :   positive
  ) ;
  port (
    clock       :   in  std_ulogic ;
    reset       :   in  std_ulogic ;

    in_sample   :   in  complex ;
    in_valid    :   in  std_ulogic ;

    out_sample  :   out complex ;
    out_valid   :   out std_ulogic
  ) ;
end entity ;

architecture arch of pfb_analysis is

    function clog2(x : in natural) return natural is
    begin
        return integer(ceil(log2(real(x)))) ;
    end function ;

    constant TAPS_PER_LEG : positive := integer(ceil(real(H'length) / real(N))) ;

    type taps_t is array(natural range 0 to N-1) of sfixed_vector(0 to TAPS_PER_LEG-1)(H'element'range) ;
    type state_t is array(natural range 0 to N-1) of complex_vector(0 to TAPS_PER_LEG-1)(re(in_sample.re'range), im(in_sample.im'range)) ;

    function reshape_taps(h : in sfixed_vector ; n : in positive) return taps_t is
        variable rv : taps_t ;
    begin
        for leg in 0 to N-1 loop
            for tap in rv(leg)'range loop
                rv(leg)(tap) := H(leg+tap*TAPS_PER_LEG) ;
            end loop ;
        end loop ;
        return rv ;
    end function ;

    constant taps : taps_t := reshape_taps(H, N) ;

    signal state : state_t ;

    signal state_valid      : std_ulogic ;
    signal prod_valid       : std_ulogic ;
    signal accum_valid      : std_ulogic ;
    signal retime_valids    : std_ulogic_vector(4 downto 0) ;

    signal state_idx : natural range 0 to N-1 ;
    signal prod_idx : natural range 0 to N-1 ;

    --subtype PRODS_RANGE is integer range sfixed_high(H(H'low),'*',in_sample.re) downto sfixed_low(H(H'low),'*',in_sample.re) ;
    subtype PRODS_RANGE is integer range sfixed_high(H(H'low)'high,H(H'low)'low,'*',in_sample.re'high,in_sample.re'low) + 1 downto sfixed_low(H(H'low)'high,H(H'low)'low,'*',in_sample.re'high,in_sample.re'low) ;

    signal prods : complex_vector(0 to TAPS_PER_LEG-1)(re(PRODS_RANGE), im(PRODS_RANGE)) ;

    subtype ACCUM_RANGE is integer range PRODS_RANGE'left+clog2(TAPS_PER_LEG)-1 downto PRODS_RANGE'right ;
    signal accum : complex(re(ACCUM_RANGE), im(ACCUM_RANGE)) ;

    signal retimed : complex_vector(retime_valids'range)(re(ACCUM_RANGE), im(ACCUM_RANGE)) ;

    -- Unknown why this is necessary ...
    --function "*"(l : in complex ; r : in unresolved_sfixed) return complex is
    --begin
    --    return complex'(l.re * r, l.im * r) ;
    --end function ;

begin

    shift_state : process(clock)
    begin
        if( rising_edge(clock) ) then
            if( reset = '1' ) then
                state_valid <= '0' ;
                state_idx <= 0 ;
            else
                state_valid <= in_valid ;
                if( in_valid = '1' ) then
                    state(state_idx) <= in_sample & state(state_idx)(0 to state(state_idx)'high-1) ;
                    state_idx <= (state_idx + 1) mod N ;
                end if ;
            end if ;
        end if ;
    end process ;

    multiply_taps : process(clock)
        variable carry : std_logic;
    begin
        if( rising_edge(clock) ) then
            if( reset = '1' ) then
                prod_valid <= '0' ;
                prod_idx <= 0 ;
            else
                prod_valid <= state_valid ;
                if( prod_valid = '1') then
                    for sample_idx in 0 to TAPS_PER_LEG-1 loop
                        --prods(sample_idx) <= state(prod_idx)(sample_idx) * taps(prod_idx)(sample_idx) ;
                        prods(sample_idx) <= state(prod_idx)(sample_idx) * taps(prod_idx)(sample_idx) ;
                    end loop ;
                    prod_idx <= (prod_idx + 1) mod N ;
                end if ;
            end if ;
        end if ;
    end process ;

    accumulate : process(clock)
        variable temp : accum'subtype ;
    begin
        if( rising_edge(clock) ) then
            if( reset = '1' ) then
                accum_valid <= '0' ;
            else
                accum_valid <= prod_valid ;
                if( prod_valid = '1' ) then
                    for sample_idx in prods'range loop
                        if( sample_idx = 0 ) then
                            temp := resize(prods(sample_idx), temp) ;
                        else
                            temp := resize(temp + prods(sample_idx), temp) ;
                        end if ;
                        accum <= temp ;
                    end loop ;
                end if ;
            end if ;
        end if ;
    end process ;

    retime : process(clock)
    begin
        if( rising_edge(clock) ) then
            if( reset = '1' ) then
                retime_valids <= (others =>'0') ;
            else
                retime_valids <= retime_valids(retime_valids'high-1 downto 0) & accum_valid ;
                if( accum_valid = '1' ) then
                    retimed <= retimed(retimed'high-1 downto 0) & accum ;
                end if ;
            end if ;
        end if ;
    end process ;

    present_output : process(clock)
    begin
        if( rising_edge(clock) ) then
            if( reset = '1' ) then
                out_valid <= '0' ;
            else
                out_valid <= retime_valids(retime_valids'high) ;
                if( retime_valids(retime_valids'high) = '1' ) then
                    out_sample <= resize(retimed(retimed'high), out_sample) ;
                end if ;
            end if ;
        end if ;
    end process ;

end architecture ;
library ieee ;
use ieee.std_logic_1164.all ;
use ieee.fixed_pkg.all ;
use work.complex_sfixed.all ;
use work.types.all ;

entity issue810 is
end entity ;

architecture test of issue810 is

    subtype SC16Q12 is integer range 3 downto -12 ;
    subtype Q18 is integer range 0 downto -18 ;

    procedure nop(signal clock : in std_ulogic ; count : in natural) is
    begin
        for x in 0 to count loop
            wait until rising_edge(clock) ;
        end loop ;
    end procedure ;

    procedure print(msg : in string) is
        variable l : std.textio.line ;
    begin
        std.textio.write(l, msg) ;
        std.textio.writeline(std.textio.output, l) ;
    end procedure ;

    constant PFB_TAPS_T12_N16 : sfixed_vector(0 to 12*16-1)(Q18) := (
        to_sfixed(                 0, Q18'left, Q18'right),
        to_sfixed(-0.000002354394734, Q18'left, Q18'right),
        to_sfixed(-0.000006049282409, Q18'left, Q18'right),
        to_sfixed(-0.000011191187403, Q18'left, Q18'right),
        to_sfixed(-0.000017772836299, Q18'left, Q18'right),
        to_sfixed(-0.000025644176017, Q18'left, Q18'right),
        to_sfixed(-0.000034489118891, Q18'left, Q18'right),
        to_sfixed(-0.000043811021434, Q18'left, Q18'right),
        to_sfixed(-0.000052929746121, Q18'left, Q18'right),
        to_sfixed(-0.000060992757947, Q18'left, Q18'right),
        to_sfixed(-0.000067002064768, Q18'left, Q18'right),
        to_sfixed(-0.000069857934829, Q18'left, Q18'right),
        to_sfixed(-0.000068419246738, Q18'left, Q18'right),
        to_sfixed(-0.000061579095308, Q18'left, Q18'right),
        to_sfixed(-0.000048352956784, Q18'left, Q18'right),
        to_sfixed(-0.000027975389321, Q18'left, Q18'right),
        to_sfixed(                 0, Q18'left, Q18'right),
        to_sfixed( 0.000035603655360, Q18'left, Q18'right),
        to_sfixed( 0.000078363362398, Q18'left, Q18'right),
        to_sfixed( 0.000127235274775, Q18'left, Q18'right),
        to_sfixed( 0.000180560205233, Q18'left, Q18'right),
        to_sfixed( 0.000236052193003, Q18'left, Q18'right),
        to_sfixed( 0.000290825314666, Q18'left, Q18'right),
        to_sfixed( 0.000341463132512, Q18'left, Q18'right),
        to_sfixed( 0.000384133107340, Q18'left, Q18'right),
        to_sfixed( 0.000414745811064, Q18'left, Q18'right),
        to_sfixed( 0.000429155962960, Q18'left, Q18'right),
        to_sfixed( 0.000423399318215, Q18'left, Q18'right),
        to_sfixed( 0.000393956421716, Q18'left, Q18'right),
        to_sfixed( 0.000338031386173, Q18'left, Q18'right),
        to_sfixed( 0.000253831354108, Q18'left, Q18'right),
        to_sfixed( 0.000140830349878, Q18'left, Q18'right),
        to_sfixed(                 0, Q18'left, Q18'right),
        to_sfixed(-0.000166010747395, Q18'left, Q18'right),
        to_sfixed(-0.000352764121257, Q18'left, Q18'right),
        to_sfixed(-0.000554005738431, Q18'left, Q18'right),
        to_sfixed(-0.000761733208870, Q18'left, Q18'right),
        to_sfixed(-0.000966369962076, Q18'left, Q18'right),
        to_sfixed(-0.001157048331608, Q18'left, Q18'right),
        to_sfixed(-0.001322000383148, Q18'left, Q18'right),
        to_sfixed(-0.001449048892038, Q18'left, Q18'right),
        to_sfixed(-0.001526184553349, Q18'left, Q18'right),
        to_sfixed(-0.001542209266509, Q18'left, Q18'right),
        to_sfixed(-0.001487419527549, Q18'left, Q18'right),
        to_sfixed(-0.001354298943282, Q18'left, Q18'right),
        to_sfixed(-0.001138184999508, Q18'left, Q18'right),
        to_sfixed(-0.000837872784442, Q18'left, Q18'right),
        to_sfixed(-0.000456117651960, Q18'left, Q18'right),
        to_sfixed(                 0, Q18'left, Q18'right),
        to_sfixed( 0.000518881455583, Q18'left, Q18'right),
        to_sfixed( 0.001084416303584, Q18'left, Q18'right),
        to_sfixed( 0.001676210195279, Q18'left, Q18'right),
        to_sfixed( 0.002270046104377, Q18'left, Q18'right),
        to_sfixed( 0.002838581176907, Q18'left, Q18'right),
        to_sfixed( 0.003352269981135, Q18'left, Q18'right),
        to_sfixed( 0.003780493181228, Q18'left, Q18'right),
        to_sfixed( 0.004092858914878, Q18'left, Q18'right),
        to_sfixed( 0.004260633042833, Q18'left, Q18'right),
        to_sfixed( 0.004258244554467, Q18'left, Q18'right),
        to_sfixed( 0.004064804333593, Q18'left, Q18'right),
        to_sfixed( 0.003665569736097, Q18'left, Q18'right),
        to_sfixed( 0.003053284446370, Q18'left, Q18'right),
        to_sfixed( 0.002229323194452, Q18'left, Q18'right),
        to_sfixed( 0.001204574328861, Q18'left, Q18'right),
        to_sfixed(                 0, Q18'left, Q18'right),
        to_sfixed(-0.001353176296439, Q18'left, Q18'right),
        to_sfixed(-0.002813692113246, Q18'left, Q18'right),
        to_sfixed(-0.004330900201825, Q18'left, Q18'right),
        to_sfixed(-0.005845858814512, Q18'left, Q18'right),
        to_sfixed(-0.007292843888749, Q18'left, Q18'right),
        to_sfixed(-0.008601249731253, Q18'left, Q18'right),
        to_sfixed(-0.009697826120807, Q18'left, Q18'right),
        to_sfixed(-0.010509182339391, Q18'left, Q18'right),
        to_sfixed(-0.010964473380595, Q18'left, Q18'right),
        to_sfixed(-0.010998171266272, Q18'left, Q18'right),
        to_sfixed(-0.010552815717736, Q18'left, Q18'right),
        to_sfixed(-0.009581633925961, Q18'left, Q18'right),
        to_sfixed(-0.008050919222966, Q18'left, Q18'right),
        to_sfixed(-0.005942063253905, Q18'left, Q18'right),
        to_sfixed(-0.003253145754319, Q18'left, Q18'right),
        to_sfixed(                 0, Q18'left, Q18'right),
        to_sfixed( 0.003783310043715, Q18'left, Q18'right),
        to_sfixed( 0.008044643549467, Q18'left, Q18'right),
        to_sfixed( 0.012714586504025, Q18'left, Q18'right),
        to_sfixed( 0.017707898760046, Q18'left, Q18'right),
        to_sfixed( 0.022925572167622, Q18'left, Q18'right),
        to_sfixed( 0.028257442299265, Q18'left, Q18'right),
        to_sfixed( 0.033585272831069, Q18'left, Q18'right),
        to_sfixed( 0.038786210984937, Q18'left, Q18'right),
        to_sfixed( 0.043736495520700, Q18'left, Q18'right),
        to_sfixed( 0.048315286392086, Q18'left, Q18'right),
        to_sfixed( 0.052408477956454, Q18'left, Q18'right),
        to_sfixed( 0.055912355944827, Q18'left, Q18'right),
        to_sfixed( 0.058736962404713, Q18'left, Q18'right),
        to_sfixed( 0.060809042421414, Q18'left, Q18'right),
        to_sfixed( 0.062074461253546, Q18'left, Q18'right),
        to_sfixed( 0.062500000000000, Q18'left, Q18'right),
        to_sfixed( 0.062074461253546, Q18'left, Q18'right),
        to_sfixed( 0.060809042421414, Q18'left, Q18'right),
        to_sfixed( 0.058736962404713, Q18'left, Q18'right),
        to_sfixed( 0.055912355944827, Q18'left, Q18'right),
        to_sfixed( 0.052408477956454, Q18'left, Q18'right),
        to_sfixed( 0.048315286392086, Q18'left, Q18'right),
        to_sfixed( 0.043736495520700, Q18'left, Q18'right),
        to_sfixed( 0.038786210984937, Q18'left, Q18'right),
        to_sfixed( 0.033585272831069, Q18'left, Q18'right),
        to_sfixed( 0.028257442299265, Q18'left, Q18'right),
        to_sfixed( 0.022925572167622, Q18'left, Q18'right),
        to_sfixed( 0.017707898760046, Q18'left, Q18'right),
        to_sfixed( 0.012714586504025, Q18'left, Q18'right),
        to_sfixed( 0.008044643549467, Q18'left, Q18'right),
        to_sfixed( 0.003783310043715, Q18'left, Q18'right),
        to_sfixed(                 0, Q18'left, Q18'right),
        to_sfixed(-0.003253145754319, Q18'left, Q18'right),
        to_sfixed(-0.005942063253905, Q18'left, Q18'right),
        to_sfixed(-0.008050919222966, Q18'left, Q18'right),
        to_sfixed(-0.009581633925961, Q18'left, Q18'right),
        to_sfixed(-0.010552815717736, Q18'left, Q18'right),
        to_sfixed(-0.010998171266272, Q18'left, Q18'right),
        to_sfixed(-0.010964473380595, Q18'left, Q18'right),
        to_sfixed(-0.010509182339391, Q18'left, Q18'right),
        to_sfixed(-0.009697826120807, Q18'left, Q18'right),
        to_sfixed(-0.008601249731253, Q18'left, Q18'right),
        to_sfixed(-0.007292843888749, Q18'left, Q18'right),
        to_sfixed(-0.005845858814512, Q18'left, Q18'right),
        to_sfixed(-0.004330900201825, Q18'left, Q18'right),
        to_sfixed(-0.002813692113246, Q18'left, Q18'right),
        to_sfixed(-0.001353176296439, Q18'left, Q18'right),
        to_sfixed(                 0, Q18'left, Q18'right),
        to_sfixed( 0.001204574328861, Q18'left, Q18'right),
        to_sfixed( 0.002229323194452, Q18'left, Q18'right),
        to_sfixed( 0.003053284446370, Q18'left, Q18'right),
        to_sfixed( 0.003665569736097, Q18'left, Q18'right),
        to_sfixed( 0.004064804333593, Q18'left, Q18'right),
        to_sfixed( 0.004258244554467, Q18'left, Q18'right),
        to_sfixed( 0.004260633042833, Q18'left, Q18'right),
        to_sfixed( 0.004092858914878, Q18'left, Q18'right),
        to_sfixed( 0.003780493181228, Q18'left, Q18'right),
        to_sfixed( 0.003352269981135, Q18'left, Q18'right),
        to_sfixed( 0.002838581176907, Q18'left, Q18'right),
        to_sfixed( 0.002270046104377, Q18'left, Q18'right),
        to_sfixed( 0.001676210195279, Q18'left, Q18'right),
        to_sfixed( 0.001084416303584, Q18'left, Q18'right),
        to_sfixed( 0.000518881455583, Q18'left, Q18'right),
        to_sfixed(                 0, Q18'left, Q18'right),
        to_sfixed(-0.000456117651960, Q18'left, Q18'right),
        to_sfixed(-0.000837872784442, Q18'left, Q18'right),
        to_sfixed(-0.001138184999508, Q18'left, Q18'right),
        to_sfixed(-0.001354298943282, Q18'left, Q18'right),
        to_sfixed(-0.001487419527549, Q18'left, Q18'right),
        to_sfixed(-0.001542209266509, Q18'left, Q18'right),
        to_sfixed(-0.001526184553349, Q18'left, Q18'right),
        to_sfixed(-0.001449048892038, Q18'left, Q18'right),
        to_sfixed(-0.001322000383148, Q18'left, Q18'right),
        to_sfixed(-0.001157048331608, Q18'left, Q18'right),
        to_sfixed(-0.000966369962076, Q18'left, Q18'right),
        to_sfixed(-0.000761733208870, Q18'left, Q18'right),
        to_sfixed(-0.000554005738431, Q18'left, Q18'right),
        to_sfixed(-0.000352764121257, Q18'left, Q18'right),
        to_sfixed(-0.000166010747395, Q18'left, Q18'right),
        to_sfixed(                 0, Q18'left, Q18'right),
        to_sfixed( 0.000140830349878, Q18'left, Q18'right),
        to_sfixed( 0.000253831354108, Q18'left, Q18'right),
        to_sfixed( 0.000338031386173, Q18'left, Q18'right),
        to_sfixed( 0.000393956421716, Q18'left, Q18'right),
        to_sfixed( 0.000423399318215, Q18'left, Q18'right),
        to_sfixed( 0.000429155962960, Q18'left, Q18'right),
        to_sfixed( 0.000414745811064, Q18'left, Q18'right),
        to_sfixed( 0.000384133107340, Q18'left, Q18'right),
        to_sfixed( 0.000341463132512, Q18'left, Q18'right),
        to_sfixed( 0.000290825314666, Q18'left, Q18'right),
        to_sfixed( 0.000236052193003, Q18'left, Q18'right),
        to_sfixed( 0.000180560205233, Q18'left, Q18'right),
        to_sfixed( 0.000127235274775, Q18'left, Q18'right),
        to_sfixed( 0.000078363362398, Q18'left, Q18'right),
        to_sfixed( 0.000035603655360, Q18'left, Q18'right),
        to_sfixed(                 0, Q18'left, Q18'right),
        to_sfixed(-0.000027975389321, Q18'left, Q18'right),
        to_sfixed(-0.000048352956784, Q18'left, Q18'right),
        to_sfixed(-0.000061579095308, Q18'left, Q18'right),
        to_sfixed(-0.000068419246738, Q18'left, Q18'right),
        to_sfixed(-0.000069857934829, Q18'left, Q18'right),
        to_sfixed(-0.000067002064768, Q18'left, Q18'right),
        to_sfixed(-0.000060992757947, Q18'left, Q18'right),
        to_sfixed(-0.000052929746121, Q18'left, Q18'right),
        to_sfixed(-0.000043811021434, Q18'left, Q18'right),
        to_sfixed(-0.000034489118891, Q18'left, Q18'right),
        to_sfixed(-0.000025644176017, Q18'left, Q18'right),
        to_sfixed(-0.000017772836299, Q18'left, Q18'right),
        to_sfixed(-0.000011191187403, Q18'left, Q18'right),
        to_sfixed(-0.000006049282409, Q18'left, Q18'right),
        to_sfixed(-0.000002354394734, Q18'left, Q18'right)
    ) ;


    signal clock : std_ulogic := '1' ;
    signal reset : std_ulogic := '1' ;

    signal in_sample : complex(re(SC16Q12), im(SC16Q12)) ;
    signal in_valid : std_ulogic := '0' ;

    signal out_sample : complex(re(SC16Q12), im(SC16Q12)) ;
    signal out_valid : std_ulogic ;

begin

    clock <= not clock after 1 ns ;

    U_pfb : entity work.pfb_analysis
      generic map (
        H           =>  PFB_TAPS_T12_N16,
        N           =>  16
      ) port map (
        clock       =>  clock,
        reset       =>  reset,

        in_sample   =>  in_sample,
        in_valid    =>  in_valid,

        out_sample  =>  out_sample,
        out_valid   =>  out_valid
      ) ;

    tb : process
    begin
        print("Starting simulation...") ;
        reset <= '1' ;
        in_valid <= '0' ;
        nop(clock, 100) ;
        reset <= '0' ;
        nop(clock, 100) ;
        in_sample <= to_complex(0.0, 0.0, in_sample) ;
        in_valid <= '1' ;
        nop(clock, 16*12*2) ;
        in_sample <= to_complex(1.0, 1.0, in_sample) ;
        nop(clock, 16*12*2) ;
        report to_string(out_sample);
        assert abs(to_real(out_sample.re) - 0.08447265625) < 0.00001;
        assert abs(to_real(out_sample.im) - 0.08447265625) < 0.00001;
        in_sample <= to_complex(0.0, 0.0, in_sample) ;
        nop(clock, 16*12*2) ;
        in_valid <= '0' ;
        report to_string(out_sample);
        assert out_sample.re = 0.0;
        assert out_sample.im = 0.0;
        nop(clock, 10) ;
        print("Done with simulation") ;
        std.env.stop ;
    end process ;

end architecture ;

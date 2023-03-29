library ieee ;
use ieee.numeric_std.all ;

entity issue616 is
end entity ;

architecture arch of issue616 is

    type cx_t is record
        re  :   signed ;
        im  :   signed ;
    end record ;

    subtype c16_t is cx_t( re(15 downto 0), im(15 downto 0) ) ;
    subtype c18_t is cx_t( re(17 downto 0), im(17 downto 0) ) ;
    subtype c34_t is cx_t( re(33 downto 0), im(33 downto 0) ) ;

    function "*"(a : c16_t ; b : c18_t) return c34_t is
        variable rv : c34_t ;
    begin
        rv.re := a.re*b.re - a.im*b.re ;
        rv.im := a.im*b.re + a.re*b.im ;
        return rv ;
    end function ;

begin

    tb : process
        constant a : c16_t := ( re => to_signed(4567, 16), im => to_signed(3210, 16) ) ;
        constant b : c18_t := ( re => to_signed(4096, 18), im => to_signed(1234, 18) ) ;
        -- Incorrect assignment sizes
        constant c : c16_t := ( re => to_signed(1111, 17), im => to_signed(2222, 16) ) ;
        variable result : c34_t ;
        --variable n : natural ;
    begin
        -- Same argument sizes
        result := a * b ;
       -- wait for 10 ns ;
        -- Opposite argument sizes
        --result := b * a ;
        wait for 10 ns ;
        wait;
    end process ;

end architecture ;

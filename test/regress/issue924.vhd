package testpkg is
  generic (
    type A ;
    val : A ;
    function to_string(x : A) return string is <> ;
  ) ;

    constant x : A := val ;

    function doit(l, r : A) return string ;

end package ;

package body testpkg is

    function doit(l, r : A) return string is
    begin
        return to_string(l) & to_string(r) ;
    end function ;

end package body ;

library ieee ;
    use ieee.std_logic_1164.all ;
    use ieee.numeric_std.all ;

entity issue924 is
end entity ;

architecture arch of issue924 is


    --package test_int is new work.testpkg generic map(A => integer, val => 12) ;
    package test_signed is new work.testpkg generic map (A => signed, val => to_signed(12, 16)) ;

begin

    tb : process
    begin
        --report test_int.doit(44, 55) ;
        assert test_signed.doit(to_signed(44, 16), to_signed(55, 16))
            = "00000000001011000000000000110111";
        std.env.stop ;
    end process ;

end architecture ;

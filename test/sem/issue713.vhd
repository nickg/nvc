package testpkg is
  generic (
    type A ;
    type B ;
    function to_string(x : A) return string is <> ;
    function to_string(x : B) return string is <>
  ) ;

    function doit(l : A ; r : B) return string ;

end package ;

package body testpkg is

    function doit(l : A ; r : B) return string is
    begin
        return to_string(A) & to_string(B) ;  -- Error
    end function ;

end package body ;

package test_int_int is new work.testpkg generic map(A => integer, B => integer) ;

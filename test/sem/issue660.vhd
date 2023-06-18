package pack is

  function blah(constant x : in integer) return integer ;

  procedure dosomething(constant x : integer) ;

end package ;

package body pack is

  function blah(x : in integer) return integer is
  begin
      return x + 1;
  end function ;

  procedure dosomething(constant x : in integer) is
  begin
  end procedure ;

end package body ;
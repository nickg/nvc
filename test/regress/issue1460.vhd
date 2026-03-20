package ArrayGenericUsagePkg is
  generic (type ArrayType is array (type is range <>) of type is private ) ;

  function CreateArray(Size : ArrayType'index ; iValue : ArrayType'element) return ArrayType ;

end package ArrayGenericUsagePkg ;

package body ArrayGenericUsagePkg is

  function CreateArray(Size : ArrayType'index ; iValue : ArrayType'element) return ArrayType is
    variable A : ArrayType(1 to Size) := (others => iValue) ;
  begin
    return A ;
  end function CreateArray ;

end package body ArrayGenericUsagePkg ;

-------------------------------------------------------------------------------

entity issue1460 is
end entity;

architecture test of issue1460 is
  package p is new work.ArrayGenericUsagePkg generic map (integer_vector);
begin

  process is
    variable v : integer_vector := p.CreateArray(3, 42);
  begin
    assert v = (42, 42, 42);
    wait;
  end process;

end architecture;

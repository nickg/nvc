package ArrayGenericUsagePkg is
  generic (type ArrayType is array (type is range <>) of type is private ) ;

  subtype ElementType is ArrayType'element ;

  type InternalArrayType is array (natural range <>) of ElementType ;

  function CreateArray(Size : integer ; iValue : ElementType) return ArrayType ;

end package ArrayGenericUsagePkg ;

package body ArrayGenericUsagePkg is

  function CreateArray(Size : integer ; iValue : ElementType) return ArrayType is
    variable A : InternalArrayType(1 to Size) := (others => iValue) ;
  begin
    return ArrayType(A) ;
  end function CreateArray ;

end package body ArrayGenericUsagePkg ;

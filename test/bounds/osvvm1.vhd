
package NameStorePkg is

  subtype integer_max is integer;

  type NameIDType is record
    ID : integer_max ;
  end record NameIDType ;

  type NameStorePType is protected
    impure function Get       (ID : integer ;  DefaultName : string := "") return string ;
  end protected NameStorePType ;

end package NameStorePkg ;

--- ///////////////////////////////////////////////////////////////////////////
--- ///////////////////////////////////////////////////////////////////////////
--- ///////////////////////////////////////////////////////////////////////////

package body NameStorePkg is

  type NameStorePType is protected body

    ------------------------------------------------------------
    impure function Get (ID : integer ; DefaultName : string := "") return string is
    ------------------------------------------------------------
    begin
      return DefaultName;
    end function Get ;

  end protected body NameStorePType ;

  shared variable NameStore : NameStorePType ;

  ------------------------------------------------------------
  impure function GetOpt (ID : NameIDType) return string is
  ------------------------------------------------------------
  begin
    return NameStore.Get(ID.ID) ;
  end function GetOpt ;

end package body NameStorePkg ;

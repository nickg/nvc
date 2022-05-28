-- From OSVVM
--

use std.textio.all ;

package NamePkg is

  type NamePType is protected
    procedure Set (NameIn : String) ;
    impure function Get (DefaultName : string := "") return string ;
    impure function GetOpt return string ;
    impure function IsSet return boolean ;
    procedure Clear ; -- clear name
    procedure Deallocate ; -- effectively alias to clear name
  end protected NamePType ;

end package NamePkg ;

package body NamePkg is
  type NamePType is protected body

    variable NamePtr   : line ;

    ------------------------------------------------------------
    procedure Set (NameIn : String) is
    ------------------------------------------------------------
    begin
      deallocate(NamePtr) ;
      NamePtr := new string'(NameIn) ;
    end procedure Set ;

    ------------------------------------------------------------
    impure function Get (DefaultName : string := "") return string is
    ------------------------------------------------------------
    begin
      if NamePtr = NULL then
        return DefaultName ;
      else
        return NamePtr.all ;
      end if ;
    end function Get ;

    ------------------------------------------------------------
    impure function GetOpt return string is
    ------------------------------------------------------------
    begin
      if NamePtr = NULL then
        return NUL & "" ;
      else
        return NamePtr.all ;
      end if ;
    end function GetOpt ;

    ------------------------------------------------------------
    impure function IsSet return boolean is
    ------------------------------------------------------------
    begin
      return NamePtr /= NULL ;
    end function IsSet ;

    ------------------------------------------------------------
    procedure Clear is  -- clear name
    ------------------------------------------------------------
    begin
      deallocate(NamePtr) ;
    end procedure Clear ;

    ------------------------------------------------------------
    procedure Deallocate is  -- clear name
    ------------------------------------------------------------
    begin
      Clear ;
    end procedure Deallocate ;

  end protected body NamePType ;

end package body NamePkg ;

-------------------------------------------------------------------------------

use work.namepkg.all;

package other_pkg is
    type otherptype is protected
        procedure do_clear;
    end protected;
end package;

package body other_pkg is

    type otherptype is protected body
        variable n : nameptype;

        procedure do_clear is
        begin
            n.clear;
        end procedure;
    end protected body;

end package body;

-------------------------------------------------------------------------------

entity link3 is
end entity;

use work.other_pkg.all;

architecture test of link3 is
    shared variable p : otherptype;
begin

    p1: process is
    begin
        p.do_clear;
        wait;
    end process;

end architecture;

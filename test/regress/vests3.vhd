
-- Copyright (C) 2001 Bill Billowitch.

-- Some of the work to develop this test suite was done with Air Force
-- support.  The Air Force and Bill Billowitch assume no
-- responsibilities for this software.

-- This file is part of VESTs (Vhdl tESTs).

-- VESTs is free software; you can redistribute it and/or modify it
-- under the terms of the GNU General Public License as published by the
-- Free Software Foundation; either version 2 of the License, or (at
-- your option) any later version.

-- VESTs is distributed in the hope that it will be useful, but WITHOUT
-- ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
-- FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
-- for more details.

-- You should have received a copy of the GNU General Public License
-- along with VESTs; if not, write to the Free Software Foundation,
-- Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

-- ---------------------------------------------------------------------
--
-- $Id: tc745.vhd,v 1.2 2001-10-26 16:29:59 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

package c01s01b01x01p05n02i00745pkg is
  type boolean_vector       is array (natural range <>) of boolean;
  type severity_level_vector    is array (natural range <>) of severity_level;
  type integer_vector       is array (natural range <>) of integer;
  type real_vector       is array (natural range <>) of real;
  type time_vector       is array (natural range <>) of time;
  type natural_vector       is array (natural range <>) of natural;
  type positive_vector    is array (natural range <>) of positive;
  type record_std_package is record
                               a: boolean;
                               b: bit;
                               c: character;
                               d: severity_level;
                               e: integer;
                               f: real;
                               g: time;
                               h: natural;
                               i: positive;
                               j: string(1 to 7);
                               k: bit_vector(0 to 3);
                             end record;

  type array_rec_std is array (integer range <>) of record_std_package;

  function F1(inp : boolean_vector)    return boolean ;
  function F2(inp : bit_vector)    return bit ;
  function F3(inp : string)       return character ;
  function F4(inp : severity_level_vector)    return severity_level ;
  function F5(inp : integer_vector)    return integer ;
  function F6(inp : real_vector)    return real ;
  function F7(inp : time_vector)    return time ;
  function F8(inp : natural_vector)    return natural ;
  function F9(inp : positive_vector)    return positive ;
  function F10(inp : array_rec_std)    return record_std_package ;

end c01s01b01x01p05n02i00745pkg;

package body c01s01b01x01p05n02i00745pkg is
  function F1(inp : boolean_vector) return boolean is
  begin
    for i in 0 to 15 loop
      assert(inp(i) = true) report"wrong initialization of S1" severity error;
    end loop;
    return false;
  end F1;
  function F2(inp : bit_vector) return bit is
  begin
    for i in 0 to 3 loop
      assert(inp(i) = '0') report"wrong initialization of S2" severity error;
    end loop;
    return '0';
  end F2;
  function F3(inp : string) return character is
  begin
    for i in 1 to 7 loop
      assert(inp(i) = 's') report"wrong initialization of S3" severity error;
    end loop;
    return 'h';
  end F3;
  function F4(inp : severity_level_vector) return severity_level is
  begin
    for i in 0 to 15 loop
      assert(inp(i) = note) report"wrong initialization of S4" severity error;
    end loop;
    return error;
  end F4;
  function F5(inp : integer_vector) return integer is
  begin
    for i in 0 to 15 loop
      assert(inp(i) = 3) report"wrong initialization of S5" severity error;
    end loop;
    return 6;
  end F5;
  function F6(inp : real_vector) return real is
  begin
    for i in 0 to 15 loop
      assert(inp(i) = 3.0) report"wrong initialization of S6" severity error;
    end loop;
    return 6.0;
  end F6;
  function F7(inp : time_vector) return time is
  begin
    for i in 0 to 15 loop
      assert(inp(i) = 3 ns) report"wrong initialization of S7" severity error;
    end loop;
    return 6 ns;
  end F7;
  function F8(inp : natural_vector) return natural is
  begin
    for i in 0 to 15 loop
      assert(inp(i) = 1) report"wrong initialization of S8" severity error;
    end loop;
    return 6;
  end F8;
  function F9(inp : positive_vector) return positive is
  begin
    for i in 0 to 15 loop
      assert(inp(i) = 1) report"wrong initialization of S9" severity error;
    end loop;
    return 6;
  end F9;
  function F10(inp : array_rec_std) return record_std_package is
  begin
    for i in 0 to 7 loop
      assert(inp(i) = (true,'1','s',note,3,3.0,3 ns, 1,1,"sssssss","0000")) report"wrong initialization of S10" severity error;
    end loop;
    return (false,'0','s',error,5,5.0,5 ns,5,5,"metrics","1100");
  end F10;
end c01s01b01x01p05n02i00745pkg;


use work.c01s01b01x01p05n02i00745pkg.all;
ENTITY vests3 IS
  generic(
    zero : integer := 0;
    one  : integer := 1;
    two  : integer := 2;
    three: integer := 3;
    four : integer := 4;
    five : integer := 5;
    six  : integer := 6;
    seven: integer := 7;
    eight: integer := 8;
    nine : integer := 9;
    fifteen:integer:= 15;
    C1 : boolean    := true;
    C2 : bit       := '1';
    C3 : character    := 's';
    C4 : severity_level:= note;
    C5 : integer    := 3;
    C6 : real       := 3.0;
    C7 : time       := 3 ns;
    C8 : natural    := 1;
    C9 : positive    := 1;
    C10 : string    := "sssssss";
    C11 : bit_vector    := B"0000";
    C48 : record_std_package := (true,'1','s',note,3,3.0,3 ns,1,1,"sssssss","0000")
    );
  port(
    S1 : boolean_vector(zero to fifteen)    := (others => C1);
    S2 : severity_level_vector(zero to fifteen)    := (others => C4);
    S3 : integer_vector(zero to fifteen)   := (others => C5);
    S4 : real_vector(zero to fifteen)   := (others => C6);
    S5 : time_vector (zero to fifteen)   := (others => C7);
    S6 : natural_vector(zero to fifteen)   := (others => C8);
    S7 : positive_vector(zero to fifteen)   := (others => C9);
    S8 : string(one to seven)      := C10;
    S9 : bit_vector(zero to three)      := C11;
    S48: array_rec_std(zero to seven)   := (others => C48)
    );
END vests3;

ARCHITECTURE c01s01b01x01p05n02i00745arch OF vests3 IS

BEGIN
  TESTING: PROCESS

    variable var1 : boolean;
    variable var4 : severity_level;
    variable var5 : integer;
    variable var6 : real;
    variable var7 : time;
    variable var8 : natural;
    variable var9 : positive;
    variable var2 : bit;
    variable var3 : character;
    variable var48: record_std_package;

  BEGIN
    var1 := F1(S1);
    var2 := F2(S9);
    var3 := F3(S8);
    var4 := F4(S2);
    var5 := F5(S3);
    var6 := F6(S4);
    var7 := F7(S5);
    var8 := F8(S6);
    var9 := F9(S7);
    var48 := F10(S48);
    wait for 1 ns;

    assert(var1 = false) report "wrong assignment in the function F1" severity error;
    assert(var2 = '0') report "wrong assignment in the function F2" severity error;
    assert(var3 = 'h') report "wrong assignment in the function F3" severity error;
    assert(var4 = error) report "wrong assignment in the function F4" severity error;
    assert(var5 = 6) report "wrong assignment in the function F5" severity error;
    assert(var6 = 6.0) report "wrong assignment in the function F6" severity error;
    assert(var7 = 6 ns) report "wrong assignment in the function F7" severity error;
    assert(var8 = 6) report "wrong assignment in the function F8" severity error;
    assert(var9 = 6) report "wrong assignment in the function F9" severity error;
    assert(var48 = (false,'0','s',error,5,5.0,5 ns,5,5,"metrics","1100")) report "wrong assignment in the function F10" severity error;

    assert NOT(    var1    = F1(S1)   and
                   var2    = F2(S9)   and
                   var3    = F3(S8)   and
                   var4    = F4(S2)   and
                   var5    = F5(S3)   and
                   var6    = F6(S4)   and
                   var7    = F7(S5)   and
                   var8    = F8(S6)   and
                   var9    = F9(S7)   and
                   var48    = F10(S48)   )
      report "***PASSED TEST: c01s01b01x01p05n02i00745"
      severity NOTE;
    assert (    var1    = F1(S1)   and
                var2    = F2(S9)   and
                var3    = F3(S8)   and
                var4    = F4(S2)   and
                var5    = F5(S3)   and
                var6    = F6(S4)   and
                var7    = F7(S5)   and
                var8    = F8(S6)   and
                var9    = F9(S7)   and
                var48    = F10(S48)   )
      report "***FAILED TEST: c01s01b01x01p05n02i00745 - Generic can be used to specify the size of ports."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c01s01b01x01p05n02i00745arch;

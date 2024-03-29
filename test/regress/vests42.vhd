
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
-- $Id: tc492.vhd,v 1.2 2001-10-26 16:29:55 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY vests42 IS
END vests42;

ARCHITECTURE c03s02b02x00p01n01i00492arch OF vests42 IS
  type etype is (one,two,three,four,five,six,seven);
  type TR is record
               i    : integer;
               b    : bit;
               bo    : boolean;
               bv    : bit_vector (0 to 3);
               r    : real;
               t    : time;
               e    : etype;
               c    : character;
             end record;
  type T1 is record
               t    : time;
               b    : bit;
               i    : integer;
               bo    : boolean;
               r    : real;
               bv    : bit_vector (0 to 3);
               e    : etype;
               c    : character;
             end record;
  function FUNC1(signal recd1: TR) return T1 is
    variable recd2:T1;
  begin
    recd2.bv    := recd1.bv;
    recd2.b    := recd1.b;
    recd2.bo    := recd1.bo;
    recd2.i    := recd1.i;
    recd2.r    := recd1.r;
    recd2.t    := recd1.t;
    recd2.e    := recd1.e;
    recd2.c    := recd1.c;
    return recd2;
  end FUNC1;
  function FUNC2(signal recd1: TR) return integer is
  begin
    return recd1.i;
  end;
  function FUNC3(signal recd1: TR) return bit is
  begin
    return recd1.b;
  end;
  function FUNC4(signal recd1: TR) return boolean is
  begin
    return recd1.bo;
  end;
  function FUNC5(signal recd1: TR) return bit_vector is
  begin
    return recd1.bv;
  end;
  function FUNC6(signal recd1: TR) return real is
  begin
    return recd1.r;
  end;
  function FUNC7(signal recd1: TR) return time is
  begin
    return recd1.t;
  end;
  function FUNC8(signal recd1: TR) return etype is
  begin
    return recd1.e;
  end;
  function FUNC9(signal recd1: TR) return character is
  begin
    return recd1.c;
  end;

  signal var1: TR;
  signal var2: T1;

BEGIN
  TESTING: PROCESS
    variable OkayCount : integer := 0;
  BEGIN
    wait for 1 ns;
    var2 <= (bv=>"0000",b=>'0',bo=>false,i=>0,r=>0.0,t=>1 ms,e=>one,c=>'a');
    var1 <= (bv=>"0001",b=>'1',bo=>true,i=>777,r=>333.767,t=>44 ms,e=>seven,c=>'%');
    wait for 1 ns;
    var2 <= FUNC1(var1);
    wait for 1 ns;
    assert var2.bv = "0001" report "var2.bv /= 0001" severity note;
    assert var2.b = '1'    report "var2.b /= 1"     severity note;
    assert var2.bo = true    report "var2.bo /= true" severity note;
    assert var2.i = 777    report "var2.i /= 777"     severity note;
    assert var2.r = 333.767 report "var2.r /= 333.767"    severity note;
    assert var2.t = 44 ms    report "var2.t /= 44 ms" severity note;
    assert var2.e = seven    report "var2.e /= seven" severity note;
    assert var2.c = '%'    report "var2.c /= c"     severity note;
    if var2 = (bv=>"0001",b=>'1',bo=>true,i=>777,r=>333.767,t=>44 ms,e=>seven,c=>'%') then
      OkayCount := OkayCount + 1;
    else
      assert false report "bad return on FUNC1" severity note;
    end if;
    var2 <= (bv=>"0000",b=>'0',bo=>false,i=>0,r=>0.0,t=>1 ms,e=>one,c=>'a');
    wait for 1 ns;
    if var2 = (bv=>"0000",b=>'0',bo=>false,i=>0,r=>0.0,t=>1 ms,e=>one,c=>'a') then
      OkayCount := OkayCount + 1;
    end if;
    var2.i    <= FUNC2(var1);
    var2.b    <= FUNC3(var1);
    var2.bo <= FUNC4(var1);
    var2.bv <= FUNC5(var1);
    var2.r    <= FUNC6(var1);
    var2.t    <= FUNC7(var1);
    var2.e    <= FUNC8(var1);
    var2.c    <= FUNC9(var1);
    wait for 1 ns;
    assert var2.bv = "0001" report "var2.bv /= 0001" severity note;
    assert var2.b = '1'    report "var2.b /= 1"     severity note;
    assert var2.bo = true    report "var2.bo /= true" severity note;
    assert var2.i = 777    report "var2.i /= 777"     severity note;
    assert var2.r = 333.767 report "var2.r /= 333.767" severity note;
    assert var2.t = 44 ms    report "var2.t /= 44 ms" severity note;
    assert var2.e = seven    report "var2.e /= seven" severity note;
    assert var2.c = '%'    report "var2.c /= c"     severity note;
    if var2 = (bv=>"0001",b=>'1',bo=>true,i=>777,r=>333.767,t=>44 ms,e=>seven,c=>'%') then
      OkayCount := OkayCount + 1;
    else
      assert false report "bad return on FUNC2-8" severity note;
    end if;
    wait for 1 ns;
    assert NOT( OkayCount = 3 )
      report "***PASSED TEST: c03s02b02x00p01n01i00492"
      severity NOTE;
    assert ( OkayCount = 3 )
      report "***FAILED TEST: c03s02b02x00p01n01i00492 - Problem assigning record subelements in function."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c03s02b02x00p01n01i00492arch;

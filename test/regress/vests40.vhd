
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
-- $Id: tc1139.vhd,v 1.2 2001-10-26 16:29:39 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY vests40 IS
END vests40;

ARCHITECTURE c06s05b00x00p05n02i01139arch OF vests40 IS

BEGIN
  TESTING: PROCESS
    type ENUM1 is (M1, M2, M3, M4, M5);
    type ABASE is array (ENUM1 range <>) of BOOLEAN;
    subtype A1 is ABASE(ENUM1 range M1 to M5);
    function F(i : integer) return ENUM1 is
    begin
      return M2;
    end F;

    function G(j : integer) return ENUM1 is
    begin
      return M4;
    end G;
    variable ii : integer;
    variable jj : integer;
    variable V1 : A1 ; -- := (others=>TRUE);
    variable V4 : A1 ; -- := (others=>TRUE);
    variable V2, V3: ENUM1;
  BEGIN
    V1(M1 to M3) := V1(F(ii) to G(jj));
    assert NOT(V1(M1 to M3)=(false,false,false))
      report "***PASSED TEST: c06s05b00x00p05n02i01139"
      severity NOTE;
    assert (V1(M1 to M3)=(false,false,false))
      report "***FAILED TEST: c06s05b00x00p05n02i01139 - Dynamic expressions are permitted in lower and upper bounds in range specifications in array slices."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c06s05b00x00p05n02i01139arch;

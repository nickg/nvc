
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
-- $Id: tc251.vhd,v 1.2 2001-10-26 16:29:48 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c03s01b02x00p04n01i00251ent IS
END c03s01b02x00p04n01i00251ent;

ARCHITECTURE c03s01b02x00p04n01i00251arch OF c03s01b02x00p04n01i00251ent IS
  type I1 is range 1 to 10;
  type I2 is range 11 to 20;
  constant V1: I1 := 1;
  constant V2: I2 := 20;
  type I5 is range V1 to V2;
BEGIN
  TESTING: PROCESS
    variable k : integer := 6;
  BEGIN
    k := 5;
    assert NOT(k=5)
      report "***PASSED TEST: c03s01b02x00p04n01i00251"
      severity NOTE;
    assert (k=5)
      report "***FAILED TEST: c03s01b02x00p04n01i00251 - Range constraints in integer type definition need not be of the same integer type."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c03s01b02x00p04n01i00251arch;


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
-- $Id: tc3144.vhd,v 1.2 2001-10-26 16:29:52 paw Exp $
-- $Revision: 1.2 $
--
-- ---------------------------------------------------------------------

ENTITY c05s02b02x00p10n01i03144ent_a IS
  generic ( g1 : real := 22.0 );
END c05s02b02x00p10n01i03144ent_a;

ARCHITECTURE c05s02b02x00p10n01i03144arch_a OF c05s02b02x00p10n01i03144ent_a IS

BEGIN
  TESTING: PROCESS
  BEGIN
    assert NOT( g1 = 22.0 )
      report "***PASSED TEST: c05s02b02x00p10n01i03144"
      severity NOTE;
    assert ( g1 = 22.0 )
      report "***FAILED TEST: c05s02b02x00p10n01i03144 - The formal generics take on implicit OPENs."
      severity ERROR;
    wait;
  END PROCESS TESTING;

END c05s02b02x00p10n01i03144arch_a;

--

ENTITY c05s02b02x00p10n01i03144ent IS
END c05s02b02x00p10n01i03144ent;

ARCHITECTURE c05s02b02x00p10n01i03144arch OF c05s02b02x00p10n01i03144ent IS
  component c05s02b02x00p10n01i03144ent_a
  end component;
  for comp1 : c05s02b02x00p10n01i03144ent_a use entity work.c05s02b02x00p10n01i03144ent_a(c05s02b02x00p10n01i03144arch_a)
    generic map(OPEN);
BEGIN
  comp1 : c05s02b02x00p10n01i03144ent_a;
END c05s02b02x00p10n01i03144arch;


configuration vests24 of c05s02b02x00p10n01i03144ent is
  for c05s02b02x00p10n01i03144arch
  end for;
end vests24;

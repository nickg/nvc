-- -----------------------------------------------------------------
--
-- Copyright 2019 IEEE P1076 WG Authors
--
-- See the LICENSE file distributed with this work for copyright and
-- licensing information and the AUTHORS file.
--
-- This file to you under the Apache License, Version 2.0 (the "License").
-- You may obtain a copy of the License at
--
--     http://www.apache.org/licenses/LICENSE-2.0
--
-- Unless required by applicable law or agreed to in writing, software
-- distributed under the License is distributed on an "AS IS" BASIS,
-- WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
-- implied.  See the License for the specific language governing
-- permissions and limitations under the License.
--
--   Title     :  Standard VHDL Synthesis Packages
--             :  (NUMERIC_BIT package body)
--             :
--   Library   :  This package shall be compiled into a library
--             :  symbolically named IEEE.
--             :
--   Developers:  IEEE DASC Synthesis Working Group,
--             :  Accellera VHDL-TC, and IEEE P1076 Working Group
--             :
--   Purpose   :  This package defines numeric types and arithmetic functions
--             :  for use with synthesis tools. Two numeric types are defined:
--             :  -- > UNSIGNED: represents an UNSIGNED number in vector form
--             :  -- > SIGNED: represents a SIGNED number in vector form
--             :  The base element type is type BIT.
--             :  The leftmost bit is treated as the most significant bit.
--             :  Signed vectors are represented in two's complement form.
--             :  This package contains overloaded arithmetic operators on
--             :  the SIGNED and UNSIGNED types. The package also contains
--             :  useful type conversions functions, clock detection
--             :  functions, and other utility functions.
--             :
--             :  If any argument to a function is a null array, a null array
--             :  is returned (exceptions, if any, are noted individually).
--
--   Note      :  This package may be modified to include additional data
--             :  required by tools, but it must in no way change the
--             :  external interfaces or simulation behavior of the
--             :  description. It is permissible to add comments and/or
--             :  attributes to the package declarations, but not to change
--             :  or delete any original lines of the package declaration.
--             :  The package body may be changed only in accordance with
--             :  the terms of Clause 16 of this standard.
--             :
-- --------------------------------------------------------------------
-- $Revision: 1220 $
-- $Date: 2008-04-10 17:16:09 +0930 (Thu, 10 Apr 2008) $
-- --------------------------------------------------------------------

library nvc;
use nvc.sim_pkg.ieee_warnings;

package body NUMERIC_BIT is

  -- null range array constants

  constant NAU : UNSIGNED(0 downto 1) := (others => '0');
  constant NAS : SIGNED(0 downto 1)   := (others => '0');

  -- implementation controls

  constant NO_WARNING : BOOLEAN := not ieee_warnings;

  -- =========================Local Subprograms =================================

  function MAXIMUM (LEFT, RIGHT: INTEGER) return INTEGER is
  begin
    if LEFT > RIGHT then return LEFT;
    else return RIGHT;
    end if;
  end MAXIMUM;

  function MINIMUM (LEFT, RIGHT: INTEGER) return INTEGER is
  begin
    if LEFT < RIGHT then return LEFT;
    else return RIGHT;
    end if;
  end MINIMUM;

  function SIGNED_NUM_BITS (ARG : INTEGER) return NATURAL is
    variable NBITS : NATURAL;
    variable N     : NATURAL;
  begin
    if ARG >= 0 then
      N := ARG;
    else
      N := -(ARG+1);
    end if;
    NBITS := 1;
    while N > 0 loop
      NBITS := NBITS+1;
      N := N / 2;
    end loop;
    return NBITS;
  end function SIGNED_NUM_BITS;

  function UNSIGNED_NUM_BITS (ARG : NATURAL) return NATURAL is
    variable NBITS : NATURAL;
    variable N     : NATURAL;
  begin
    N := ARG;
    NBITS := 1;
    while N > 1 loop
      NBITS := NBITS+1;
      N := N / 2;
    end loop;
    return NBITS;
  end function UNSIGNED_NUM_BITS;

  ------------------------------------------------------------------------------
  -- this internal function computes the addition of two UNSIGNED
  -- with input carry
  -- * the two arguments are of the same length

  function ADD_UNSIGNED (L, R : UNSIGNED; C : BIT) return UNSIGNED is
    constant L_LEFT : INTEGER := L'length-1;
    alias XL        : UNSIGNED(L_LEFT downto 0) is L;
    alias XR        : UNSIGNED(L_LEFT downto 0) is R;
    variable RESULT : UNSIGNED(L_LEFT downto 0);
    variable CBIT   : BIT     := C;
  begin
    for I in 0 to L_LEFT loop
      RESULT(I) := CBIT xor XL(I) xor XR(I);
      CBIT      := (CBIT and XL(I)) or (CBIT and XR(I)) or (XL(I) and XR(I));
    end loop;
    return RESULT;
  end function ADD_UNSIGNED;

  -- this internal function computes the addition of two SIGNED
  -- with input carry
  -- * the two arguments are of the same length

  function ADD_SIGNED (L, R : SIGNED; C : BIT) return SIGNED is
    constant L_LEFT : INTEGER := L'length-1;
    alias XL        : SIGNED(L_LEFT downto 0) is L;
    alias XR        : SIGNED(L_LEFT downto 0) is R;
    variable RESULT : SIGNED(L_LEFT downto 0);
    variable CBIT   : BIT     := C;
  begin
    for I in 0 to L_LEFT loop
      RESULT(I) := CBIT xor XL(I) xor XR(I);
      CBIT      := (CBIT and XL(I)) or (CBIT and XR(I)) or (XL(I) and XR(I));
    end loop;
    return RESULT;
  end function ADD_SIGNED;

  ------------------------------------------------------------------------------

  -- this internal procedure computes UNSIGNED division
  -- giving the quotient and remainder.
  procedure DIVMOD (NUM, XDENOM : UNSIGNED; XQUOT, XREMAIN : out UNSIGNED) is
    variable TEMP   : UNSIGNED(NUM'length downto 0);
    variable QUOT   : UNSIGNED(MAXIMUM(NUM'length, XDENOM'length)-1 downto 0);
    alias DENOM     : UNSIGNED(XDENOM'length-1 downto 0) is XDENOM;
    variable TOPBIT : INTEGER;
  begin
    TEMP   := "0"&NUM;
    QUOT   := (others => '0');
    TOPBIT := -1;
    for J in DENOM'range loop
      if DENOM(J) = '1' then
        TOPBIT := J;
        exit;
      end if;
    end loop;
    assert TOPBIT >= 0 report "NUMERIC_BIT.DIVMOD: DIV, MOD, or REM by zero"
      severity error;

    for J in NUM'length-(TOPBIT+1) downto 0 loop
      if TEMP(TOPBIT+J+1 downto J) >= "0"&DENOM(TOPBIT downto 0) then
        TEMP(TOPBIT+J+1 downto J) := (TEMP(TOPBIT+J+1 downto J))
                                     -("0"&DENOM(TOPBIT downto 0));
        QUOT(J) := '1';
      end if;
      assert TEMP(TOPBIT+J+1) = '0'
        report "NUMERIC_BIT.DIVMOD: internal error in the division algorithm"
        severity error;
    end loop;
    XQUOT   := RESIZE(QUOT, XQUOT'length);
    XREMAIN := RESIZE(TEMP, XREMAIN'length);
  end procedure DIVMOD;

  -----------------Local Subprograms - shift/rotate ops-------------------------

  function XSLL (ARG : BIT_VECTOR; COUNT : NATURAL) return BIT_VECTOR is
    constant ARG_L  : INTEGER                    := ARG'length-1;
    alias XARG      : BIT_VECTOR(ARG_L downto 0) is ARG;
    variable RESULT : BIT_VECTOR(ARG_L downto 0) := (others => '0');
  begin
    if COUNT <= ARG_L then
      RESULT(ARG_L downto COUNT) := XARG(ARG_L-COUNT downto 0);
    end if;
    return RESULT;
  end function XSLL;

  function XSRL (ARG : BIT_VECTOR; COUNT : NATURAL) return BIT_VECTOR is
    constant ARG_L  : INTEGER                    := ARG'length-1;
    alias XARG      : BIT_VECTOR(ARG_L downto 0) is ARG;
    variable RESULT : BIT_VECTOR(ARG_L downto 0) := (others => '0');
  begin
    if COUNT <= ARG_L then
      RESULT(ARG_L-COUNT downto 0) := XARG(ARG_L downto COUNT);
    end if;
    return RESULT;
  end function XSRL;

  function XSRA (ARG : BIT_VECTOR; COUNT : NATURAL) return BIT_VECTOR is
    constant ARG_L  : INTEGER := ARG'length-1;
    alias XARG      : BIT_VECTOR(ARG_L downto 0) is ARG;
    variable RESULT : BIT_VECTOR(ARG_L downto 0);
    variable XCOUNT : NATURAL := COUNT;
  begin
    if ((ARG'length <= 1) or (XCOUNT = 0)) then return ARG;
    else
      if (XCOUNT > ARG_L) then XCOUNT           := ARG_L;
      end if;
      RESULT(ARG_L-XCOUNT downto 0)             := XARG(ARG_L downto XCOUNT);
      RESULT(ARG_L downto (ARG_L - XCOUNT + 1)) := (others => XARG(ARG_L));
    end if;
    return RESULT;
  end function XSRA;

  function XROL (ARG : BIT_VECTOR; COUNT : NATURAL) return BIT_VECTOR is
    constant ARG_L  : INTEGER                    := ARG'length-1;
    alias XARG      : BIT_VECTOR(ARG_L downto 0) is ARG;
    variable RESULT : BIT_VECTOR(ARG_L downto 0) := XARG;
    variable COUNTM : INTEGER;
  begin
    COUNTM := COUNT mod (ARG_L + 1);
    if COUNTM /= 0 then
      RESULT(ARG_L downto COUNTM) := XARG(ARG_L-COUNTM downto 0);
      RESULT(COUNTM-1 downto 0)   := XARG(ARG_L downto ARG_L-COUNTM+1);
    end if;
    return RESULT;
  end function XROL;

  function XROR (ARG : BIT_VECTOR; COUNT : NATURAL) return BIT_VECTOR is
    constant ARG_L  : INTEGER                    := ARG'length-1;
    alias XARG      : BIT_VECTOR(ARG_L downto 0) is ARG;
    variable RESULT : BIT_VECTOR(ARG_L downto 0) := XARG;
    variable COUNTM : INTEGER;
  begin
    COUNTM := COUNT mod (ARG_L + 1);
    if COUNTM /= 0 then
      RESULT(ARG_L-COUNTM downto 0)       := XARG(ARG_L downto COUNTM);
      RESULT(ARG_L downto ARG_L-COUNTM+1) := XARG(COUNTM-1 downto 0);
    end if;
    return RESULT;
  end function XROR;

  ---------------- Local Subprograms - Relational Operators --------------------

  --
  -- General "=" for UNSIGNED vectors, same length
  --
  function UNSIGNED_EQUAL (L, R : UNSIGNED) return BOOLEAN is
  begin
    return BIT_VECTOR(L) = BIT_VECTOR(R);
  end function UNSIGNED_EQUAL;

  --
  -- General "=" for SIGNED vectors, same length
  --
  function SIGNED_EQUAL (L, R : SIGNED) return BOOLEAN is
  begin
    return BIT_VECTOR(L) = BIT_VECTOR(R);
  end function SIGNED_EQUAL;

  --
  -- General "<" for UNSIGNED vectors, same length
  --
  function UNSIGNED_LESS (L, R : UNSIGNED) return BOOLEAN is
  begin
    return BIT_VECTOR(L) < BIT_VECTOR(R);
  end function UNSIGNED_LESS;

  --
  -- General "<" function for SIGNED vectors, same length
  --
  function SIGNED_LESS (L, R : SIGNED) return BOOLEAN is
    -- Need aliases to assure index direction
    variable INTERN_L : SIGNED(0 to L'length-1);
    variable INTERN_R : SIGNED(0 to R'length-1);
  begin
    INTERN_L    := L;
    INTERN_R    := R;
    INTERN_L(0) := not INTERN_L(0);
    INTERN_R(0) := not INTERN_R(0);
    return BIT_VECTOR(INTERN_L) < BIT_VECTOR(INTERN_R);
  end function SIGNED_LESS;

  --
  -- General "<=" function for UNSIGNED vectors, same length
  --
  function UNSIGNED_LESS_OR_EQUAL (L, R : UNSIGNED) return BOOLEAN is
  begin
    return BIT_VECTOR(L) <= BIT_VECTOR(R);
  end function UNSIGNED_LESS_OR_EQUAL;

  --
  -- General "<=" function for SIGNED vectors, same length
  --
  function SIGNED_LESS_OR_EQUAL (L, R : SIGNED) return BOOLEAN is
    -- Need aliases to assure index direction
    variable INTERN_L : SIGNED(0 to L'length-1);
    variable INTERN_R : SIGNED(0 to R'length-1);
  begin
    INTERN_L                    := L;
    INTERN_R                    := R;
    INTERN_L(0)                 := not INTERN_L(0);
    INTERN_R(0)                 := not INTERN_R(0);
    return BIT_VECTOR(INTERN_L) <= BIT_VECTOR(INTERN_R);
  end function SIGNED_LESS_OR_EQUAL;

  -- ====================== Exported Functions ==================================

  -- Id: A.1
  function "abs" (ARG : SIGNED) return SIGNED is
    constant ARG_LEFT : INTEGER := ARG'length-1;
    variable RESULT   : SIGNED(ARG_LEFT downto 0);
  begin
    if ARG'length < 1 then return NAS;
    end if;
    RESULT := ARG;
    if RESULT(RESULT'left) = '1' then
      RESULT := -RESULT;
    end if;
    return RESULT;
  end function "abs";

  -- Id: A.2
  function "-" (ARG : SIGNED) return SIGNED is
    constant ARG_LEFT : INTEGER := ARG'length-1;
    alias XARG        : SIGNED(ARG_LEFT downto 0) is ARG;
    variable RESULT   : SIGNED(ARG_LEFT downto 0);
    variable CBIT     : BIT     := '1';
  begin
    if ARG'length < 1 then return NAS;
    end if;
    for I in 0 to RESULT'left loop
      RESULT(I) := not(XARG(I)) xor CBIT;
      CBIT      := CBIT and not(XARG(I));
    end loop;
    return RESULT;
  end function "-";

  -- ============================================================================

  -- Id: A.3
  function "+" (L, R : UNSIGNED) return UNSIGNED is
    constant SIZE   : NATURAL := MAXIMUM(L'length, R'length);
  begin
    if ((L'length < 1) or (R'length < 1)) then return NAU;
    end if;
    return ADD_UNSIGNED(RESIZE(L, SIZE), RESIZE(R, SIZE), '0');
  end function "+";

  -- Id: A.4
  function "+" (L, R : SIGNED) return SIGNED is
    constant SIZE   : NATURAL := MAXIMUM(L'length, R'length);
  begin
    if ((L'length < 1) or (R'length < 1)) then return NAS;
    end if;
    return ADD_SIGNED(RESIZE(L, SIZE), RESIZE(R, SIZE), '0');
  end function "+";

  -- Id: A.5
  function "+" (L : UNSIGNED; R : NATURAL) return UNSIGNED is
  begin
    return L + TO_UNSIGNED(R, L'length);
  end function "+";

  -- Id: A.6
  function "+" (L : NATURAL; R : UNSIGNED) return UNSIGNED is
  begin
    return TO_UNSIGNED(L, R'length) + R;
  end function "+";

  -- Id: A.7
  function "+" (L : SIGNED; R : INTEGER) return SIGNED is
  begin
    return L + TO_SIGNED(R, L'length);
  end function "+";

  -- Id: A.8
  function "+" (L : INTEGER; R : SIGNED) return SIGNED is
  begin
    return TO_SIGNED(L, R'length) + R;
  end function "+";

  -- ============================================================================

  -- Id: A.9
  function "-" (L, R : UNSIGNED) return UNSIGNED is
    constant SIZE   : NATURAL := MAXIMUM(L'length, R'length);
  begin
    if ((L'length < 1) or (R'length < 1)) then return NAU;
    end if;
    return ADD_UNSIGNED(RESIZE(L, SIZE),
                        not(RESIZE(R, SIZE)),
                        '1');
  end function "-";

  -- Id: A.10
  function "-" (L, R : SIGNED) return SIGNED is
    constant SIZE   : NATURAL := MAXIMUM(L'length, R'length);
  begin
    if ((L'length < 1) or (R'length < 1)) then return NAS;
    end if;
    return ADD_SIGNED(RESIZE(L, SIZE),
                      not(RESIZE(R, SIZE)),
                      '1');
  end function "-";

  -- Id: A.11
  function "-" (L : UNSIGNED; R : NATURAL) return UNSIGNED is
  begin
    return L - TO_UNSIGNED(R, L'length);
  end function "-";

  -- Id: A.12
  function "-" (L : NATURAL; R : UNSIGNED) return UNSIGNED is
  begin
    return TO_UNSIGNED(L, R'length) - R;
  end function "-";

  -- Id: A.13
  function "-" (L : SIGNED; R : INTEGER) return SIGNED is
  begin
    return L - TO_SIGNED(R, L'length);
  end function "-";

  -- Id: A.14
  function "-" (L : INTEGER; R : SIGNED) return SIGNED is
  begin
    return TO_SIGNED(L, R'length) - R;
  end function "-";

  -- ============================================================================

  -- Id: A.15
  function "*" (L, R : UNSIGNED) return UNSIGNED is
    constant L_LEFT : INTEGER                                  := L'length-1;
    constant R_LEFT : INTEGER                                  := R'length-1;
    alias XL        : UNSIGNED(L_LEFT downto 0) is L;
    alias XR        : UNSIGNED(R_LEFT downto 0) is R;
    variable RESULT : UNSIGNED((L'length+R'length-1) downto 0) := (others => '0');
    variable ADVAL  : UNSIGNED((L'length+R'length-1) downto 0);
  begin
    if ((L'length < 1) or (R'length < 1)) then return NAU;
    end if;
    ADVAL := RESIZE(XR, RESULT'length);
    for I in 0 to L_LEFT loop
      if XL(I) = '1' then RESULT := RESULT + ADVAL;
      end if;
      ADVAL                      := SHIFT_LEFT(ADVAL, 1);
    end loop;
    return RESULT;
  end function "*";

  -- Id: A.16
  function "*" (L, R : SIGNED) return SIGNED is
    constant L_LEFT : INTEGER                            := L'length-1;
    constant R_LEFT : INTEGER                            := R'length-1;
    variable XL     : SIGNED(L_LEFT downto 0);
    variable XR     : SIGNED(R_LEFT downto 0);
    variable RESULT : SIGNED((L_LEFT+R_LEFT+1) downto 0) := (others => '0');
    variable ADVAL  : SIGNED((L_LEFT+R_LEFT+1) downto 0);
  begin
    if ((L_LEFT < 0) or (R_LEFT < 0)) then return NAS;
    end if;
    XL    := L;
    XR    := R;
    ADVAL := RESIZE(XR, RESULT'length);
    for I in 0 to L_LEFT-1 loop
      if XL(I) = '1' then RESULT := RESULT + ADVAL;
      end if;
      ADVAL                      := SHIFT_LEFT(ADVAL, 1);
    end loop;
    if XL(L_LEFT) = '1' then
      RESULT := RESULT - ADVAL;
    end if;
    return RESULT;
  end function "*";

  -- Id: A.17
  function "*" (L : UNSIGNED; R : NATURAL) return UNSIGNED is
  begin
    return L * TO_UNSIGNED(R, L'length);
  end function "*";

  -- Id: A.18
  function "*" (L : NATURAL; R : UNSIGNED) return UNSIGNED is
  begin
    return TO_UNSIGNED(L, R'length) * R;
  end function "*";

  -- Id: A.19
  function "*" (L : SIGNED; R : INTEGER) return SIGNED is
  begin
    return L * TO_SIGNED(R, L'length);
  end function "*";

  -- Id: A.20
  function "*" (L : INTEGER; R : SIGNED) return SIGNED is
  begin
    return TO_SIGNED(L, R'length) * R;
  end function "*";

  -- ============================================================================

  -- Id: A.21
  function "/" (L, R : UNSIGNED) return UNSIGNED is
    variable FQUOT   : UNSIGNED(L'length-1 downto 0);
    variable FREMAIN : UNSIGNED(R'length-1 downto 0);
  begin
    if ((L'length < 1) or (R'length < 1)) then return NAU;
    end if;
    DIVMOD(L, R, FQUOT, FREMAIN);
    return FQUOT;
  end function "/";

  -- Id: A.22
  function "/" (L, R : SIGNED) return SIGNED is
    variable FQUOT   : UNSIGNED(L'length-1 downto 0);
    variable FREMAIN : UNSIGNED(R'length-1 downto 0);
    variable XNUM    : UNSIGNED(L'length-1 downto 0);
    variable XDENOM  : UNSIGNED(R'length-1 downto 0);
    variable QNEG    : BOOLEAN := false;
  begin
    if ((L'length < 1) or (R'length < 1)) then return NAS;
    end if;
    if L(L'left) = '1' then
      XNUM := UNSIGNED(-L);
      QNEG := true;
    else
      XNUM := UNSIGNED(L);
    end if;
    if R(R'left) = '1' then
      XDENOM := UNSIGNED(-R);
      QNEG   := not QNEG;
    else
      XDENOM := UNSIGNED(R);
    end if;
    DIVMOD(XNUM, XDENOM, FQUOT, FREMAIN);
    if QNEG then FQUOT := "0"-FQUOT;
    end if;
    return SIGNED(FQUOT);
  end function "/";

  -- Id: A.23
  function "/" (L : UNSIGNED; R : NATURAL) return UNSIGNED is
    constant R_LENGTH : NATURAL := MAXIMUM(L'length, UNSIGNED_NUM_BITS(R));
    variable XR, QUOT : UNSIGNED(R_LENGTH-1 downto 0);
  begin
    if (L'length < 1) then return NAU;
    end if;
    if (R_LENGTH > L'length) then
      QUOT := (others => '0');
      return RESIZE(QUOT, L'length);
    end if;
    XR   := TO_UNSIGNED(R, R_LENGTH);
    QUOT := RESIZE((L / XR), QUOT'length);
    return RESIZE(QUOT, L'length);
  end function "/";

  -- Id: A.24
  function "/" (L : NATURAL; R : UNSIGNED) return UNSIGNED is
    constant L_LENGTH : NATURAL := MAXIMUM(UNSIGNED_NUM_BITS(L), R'length);
    variable XL, QUOT : UNSIGNED(L_LENGTH-1 downto 0);
  begin
    if (R'length < 1) then return NAU;
    end if;
    XL   := TO_UNSIGNED(L, L_LENGTH);
    QUOT := RESIZE((XL / R), QUOT'length);
    if L_LENGTH > R'length
      and QUOT(L_LENGTH-1 downto R'length)
      /= (L_LENGTH-1 downto R'length => '0')
    then
      assert NO_WARNING report "NUMERIC_BIT.""/"": Quotient Truncated"
        severity warning;
    end if;
    return RESIZE(QUOT, R'length);
  end function "/";

  -- Id: A.25
  function "/" (L : SIGNED; R : INTEGER) return SIGNED is
    constant R_LENGTH : NATURAL := MAXIMUM(L'length, SIGNED_NUM_BITS(R));
    variable XR, QUOT : SIGNED(R_LENGTH-1 downto 0);
  begin
    if (L'length < 1) then return NAS;
    end if;
    if (R_LENGTH > L'length) then
      QUOT := (others => '0');
      return RESIZE(QUOT, L'length);
    end if;
    XR   := TO_SIGNED(R, R_LENGTH);
    QUOT := RESIZE((L / XR), QUOT'length);
    return RESIZE(QUOT, L'length);
  end function "/";

  -- Id: A.26
  function "/" (L : INTEGER; R : SIGNED) return SIGNED is
    constant L_LENGTH : NATURAL := MAXIMUM(SIGNED_NUM_BITS(L), R'length);
    variable XL, QUOT : SIGNED(L_LENGTH-1 downto 0);
  begin
    if (R'length < 1) then return NAS;
    end if;
    XL   := TO_SIGNED(L, L_LENGTH);
    QUOT := RESIZE((XL / R), QUOT'length);
    if L_LENGTH > R'length and QUOT(L_LENGTH-1 downto R'length)
      /= (L_LENGTH-1 downto R'length => QUOT(R'length-1))
    then
      assert NO_WARNING report "NUMERIC_BIT.""/"": Quotient Truncated"
        severity warning;
    end if;
    return RESIZE(QUOT, R'length);
  end function "/";

  -- ============================================================================

  -- Id: A.27
  function "rem" (L, R : UNSIGNED) return UNSIGNED is
    variable FQUOT   : UNSIGNED(L'length-1 downto 0);
    variable FREMAIN : UNSIGNED(R'length-1 downto 0);
  begin
    if ((L'length < 1) or (R'length < 1)) then return NAU;
    end if;
    DIVMOD(L, R, FQUOT, FREMAIN);
    return FREMAIN;
  end function "rem";

  -- Id: A.28
  function "rem" (L, R : SIGNED) return SIGNED is
    variable FQUOT   : UNSIGNED(L'length-1 downto 0);
    variable FREMAIN : UNSIGNED(R'length-1 downto 0);
    variable XNUM    : UNSIGNED(L'length-1 downto 0);
    variable XDENOM  : UNSIGNED(R'length-1 downto 0);
    variable RNEG    : BOOLEAN := false;
  begin
    if ((L'length < 1) or (R'length < 1)) then return NAS;
    end if;
    if L(L'left) = '1' then
      XNUM := UNSIGNED(-L);
      RNEG := true;
    else
      XNUM := UNSIGNED(L);
    end if;
    if R(R'left) = '1' then
      XDENOM := UNSIGNED(-R);
    else
      XDENOM := UNSIGNED(R);
    end if;
    DIVMOD(XNUM, XDENOM, FQUOT, FREMAIN);
    if RNEG then
      FREMAIN := "0"-FREMAIN;
    end if;
    return SIGNED(FREMAIN);
  end function "rem";

  -- Id: A.29
  function "rem" (L : UNSIGNED; R : NATURAL) return UNSIGNED is
    constant R_LENGTH : NATURAL := MAXIMUM(L'length, UNSIGNED_NUM_BITS(R));
    variable XR, XREM : UNSIGNED(R_LENGTH-1 downto 0);
  begin
    if (L'length < 1) then return NAU;
    end if;
    XR   := TO_UNSIGNED(R, R_LENGTH);
    XREM := RESIZE((L rem XR), XREM'length);
    if R_LENGTH > L'length and XREM(R_LENGTH-1 downto L'length)
      /= (R_LENGTH-1 downto L'length => '0')
    then
      assert NO_WARNING report "NUMERIC_BIT.""rem"": Remainder Truncated"
        severity warning;
    end if;
    return RESIZE(XREM, L'length);
  end function "rem";

  -- Id: A.30
  function "rem" (L : NATURAL; R : UNSIGNED) return UNSIGNED is
    constant L_LENGTH : NATURAL := MAXIMUM(UNSIGNED_NUM_BITS(L), R'length);
    variable XL, XREM : UNSIGNED(L_LENGTH-1 downto 0);
  begin
    if (R'length < 1) then return NAU;
    end if;
    XL   := TO_UNSIGNED(L, L_LENGTH);
    XREM := RESIZE((XL rem R), XREM'length);
    if L_LENGTH > R'length and XREM(L_LENGTH-1 downto R'length)
      /= (L_LENGTH-1 downto R'length => '0')
    then
      assert NO_WARNING report "NUMERIC_BIT.""rem"": Remainder Truncated"
        severity warning;
    end if;
    return RESIZE(XREM, R'length);
  end function "rem";

  -- Id: A.31
  function "rem" (L : SIGNED; R : INTEGER) return SIGNED is
    constant R_LENGTH : NATURAL := MAXIMUM(L'length, SIGNED_NUM_BITS(R));
    variable XR, XREM : SIGNED(R_LENGTH-1 downto 0);
  begin
    if (L'length < 1) then return NAS;
    end if;
    XR   := TO_SIGNED(R, R_LENGTH);
    XREM := RESIZE((L rem XR), XREM'length);
    if R_LENGTH > L'length and XREM(R_LENGTH-1 downto L'length)
      /= (R_LENGTH-1 downto L'length => XREM(L'length-1))
    then
      assert NO_WARNING report "NUMERIC_BIT.""rem"": Remainder Truncated"
        severity warning;
    end if;
    return RESIZE(XREM, L'length);
  end function "rem";

  -- Id: A.32
  function "rem" (L : INTEGER; R : SIGNED) return SIGNED is
    constant L_LENGTH : NATURAL := MAXIMUM(SIGNED_NUM_BITS(L), R'length);
    variable XL, XREM : SIGNED(L_LENGTH-1 downto 0);
  begin
    if (R'length < 1) then return NAS;
    end if;
    XL   := TO_SIGNED(L, L_LENGTH);
    XREM := RESIZE((XL rem R), XREM'length);
    if L_LENGTH > R'length and XREM(L_LENGTH-1 downto R'length)
      /= (L_LENGTH-1 downto R'length => XREM(R'length-1))
    then
      assert NO_WARNING report "NUMERIC_BIT.""rem"": Remainder Truncated"
        severity warning;
    end if;
    return RESIZE(XREM, R'length);
  end function "rem";

  -- ============================================================================

  -- Id: A.33
  function "mod" (L, R : UNSIGNED) return UNSIGNED is
    variable FQUOT   : UNSIGNED(L'length-1 downto 0);
    variable FREMAIN : UNSIGNED(R'length-1 downto 0);
  begin
    if ((L'length < 1) or (R'length < 1)) then return NAU;
    end if;
    DIVMOD(L, R, FQUOT, FREMAIN);
    return FREMAIN;
  end function "mod";

  -- Id: A.34
  function "mod" (L, R : SIGNED) return SIGNED is
    variable FQUOT   : UNSIGNED(L'length-1 downto 0);
    variable FREMAIN : UNSIGNED(R'length-1 downto 0);
    variable XNUM    : UNSIGNED(L'length-1 downto 0);
    variable XDENOM  : UNSIGNED(R'length-1 downto 0);
    variable RNEG    : BOOLEAN := false;
  begin
    if ((L'length < 1) or (R'length < 1)) then return NAS;
    end if;
    if L(L'left) = '1' then
      XNUM := UNSIGNED(-L);
    else
      XNUM := UNSIGNED(L);
    end if;
    if R(R'left) = '1' then
      XDENOM := UNSIGNED(-R);
      RNEG   := true;
    else
      XDENOM := UNSIGNED(R);
    end if;
    DIVMOD(XNUM, XDENOM, FQUOT, FREMAIN);
    if RNEG and L(L'left) = '1' then
      FREMAIN := "0"-FREMAIN;
    elsif RNEG and FREMAIN /= "0" then
      FREMAIN := FREMAIN-XDENOM;
    elsif L(L'left) = '1' and FREMAIN /= "0" then
      FREMAIN := XDENOM-FREMAIN;
    end if;
    return SIGNED(FREMAIN);
  end function "mod";

  -- Id: A.35
  function "mod" (L : UNSIGNED; R : NATURAL) return UNSIGNED is
    constant R_LENGTH : NATURAL := MAXIMUM(L'length, UNSIGNED_NUM_BITS(R));
    variable XR, XREM : UNSIGNED(R_LENGTH-1 downto 0);
  begin
    if (L'length < 1) then return NAU;
    end if;
    XR   := TO_UNSIGNED(R, R_LENGTH);
    XREM := RESIZE((L mod XR), XREM'length);
    if R_LENGTH > L'length and XREM(R_LENGTH-1 downto L'length)
      /= (R_LENGTH-1 downto L'length => '0')
    then
      assert NO_WARNING report "NUMERIC_BIT.""mod"": Modulus Truncated"
        severity warning;
    end if;
    return RESIZE(XREM, L'length);
  end function "mod";

  -- Id: A.36
  function "mod" (L : NATURAL; R : UNSIGNED) return UNSIGNED is
    constant L_LENGTH : NATURAL := MAXIMUM(UNSIGNED_NUM_BITS(L), R'length);
    variable XL, XREM : UNSIGNED(L_LENGTH-1 downto 0);
  begin
    if (R'length < 1) then return NAU;
    end if;
    XL   := TO_UNSIGNED(L, L_LENGTH);
    XREM := RESIZE((XL mod R), XREM'length);
    if L_LENGTH > R'length and XREM(L_LENGTH-1 downto R'length)
      /= (L_LENGTH-1 downto R'length => '0')
    then
      assert NO_WARNING report "NUMERIC_BIT.""mod"": Modulus Truncated"
        severity warning;
    end if;
    return RESIZE(XREM, R'length);
  end function "mod";

  -- Id: A.37
  function "mod" (L : SIGNED; R : INTEGER) return SIGNED is
    constant R_LENGTH : NATURAL := MAXIMUM(L'length, SIGNED_NUM_BITS(R));
    variable XR, XREM : SIGNED(R_LENGTH-1 downto 0);
  begin
    if (L'length < 1) then return NAS;
    end if;
    XR   := TO_SIGNED(R, R_LENGTH);
    XREM := RESIZE((L mod XR), XREM'length);
    if R_LENGTH > L'length and XREM(R_LENGTH-1 downto L'length)
      /= (R_LENGTH-1 downto L'length => XREM(L'length-1))
    then
      assert NO_WARNING report "NUMERIC_BIT.""mod"": Modulus Truncated"
        severity warning;
    end if;
    return RESIZE(XREM, L'length);
  end function "mod";

  -- Id: A.38
  function "mod" (L : INTEGER; R : SIGNED) return SIGNED is
    constant L_LENGTH : NATURAL := MAXIMUM(SIGNED_NUM_BITS(L), R'length);
    variable XL, XREM : SIGNED(L_LENGTH-1 downto 0);
  begin
    if (R'length < 1) then return NAS;
    end if;
    XL   := TO_SIGNED(L, L_LENGTH);
    XREM := RESIZE((XL mod R), XREM'length);
    if L_LENGTH > R'length and XREM(L_LENGTH-1 downto R'length)
      /= (L_LENGTH-1 downto R'length => XREM(R'length-1))
    then
      assert NO_WARNING report "NUMERIC_BIT.""mod"": Modulus Truncated"
        severity warning;
    end if;
    return RESIZE(XREM, R'length);
  end function "mod";

  -- ============================================================================

  -- Id: C.1
  function ">" (L, R : UNSIGNED) return BOOLEAN is
    constant SIZE : NATURAL := MAXIMUM(L'length, R'length);
  begin
    if ((L'length < 1) or (R'length < 1)) then
      assert NO_WARNING
        report "NUMERIC_BIT."">"": null argument detected, returning FALSE"
        severity warning;
      return false;
    end if;
    return not UNSIGNED_LESS_OR_EQUAL(RESIZE(L, SIZE), RESIZE(R, SIZE));
  end function ">";

  -- Id: C.2
  function ">" (L, R : SIGNED) return BOOLEAN is
    constant SIZE : NATURAL := MAXIMUM(L'length, R'length);
  begin
    if ((L'length < 1) or (R'length < 1)) then
      assert NO_WARNING
        report "NUMERIC_BIT."">"": null argument detected, returning FALSE"
        severity warning;
      return false;
    end if;
    return not SIGNED_LESS_OR_EQUAL(RESIZE(L, SIZE), RESIZE(R, SIZE));
  end function ">";

  -- Id: C.3
  function ">" (L : NATURAL; R : UNSIGNED) return BOOLEAN is
  begin
    if (R'length < 1) then
      assert NO_WARNING
        report "NUMERIC_BIT."">"": null argument detected, returning FALSE"
        severity warning;
      return false;
    end if;
    if UNSIGNED_NUM_BITS(L) > R'length then return true;
    end if;
    return not UNSIGNED_LESS_OR_EQUAL(TO_UNSIGNED(L, R'length), R);
  end function ">";

  -- Id: C.4
  function ">" (L : INTEGER; R : SIGNED) return BOOLEAN is
  begin
    if (R'length < 1) then
      assert NO_WARNING
        report "NUMERIC_BIT."">"": null argument detected, returning FALSE"
        severity warning;
      return false;
    end if;
    if SIGNED_NUM_BITS(L) > R'length then return L > 0;
    end if;
    return not SIGNED_LESS_OR_EQUAL(TO_SIGNED(L, R'length), R);
  end function ">";

  -- Id: C.5
  function ">" (L : UNSIGNED; R : NATURAL) return BOOLEAN is
  begin
    if (L'length < 1) then
      assert NO_WARNING
        report "NUMERIC_BIT."">"": null argument detected, returning FALSE"
        severity warning;
      return false;
    end if;
    if UNSIGNED_NUM_BITS(R) > L'length then return false;
    end if;
    return not UNSIGNED_LESS_OR_EQUAL(L, TO_UNSIGNED(R, L'length));
  end function ">";

  -- Id: C.6
  function ">" (L : SIGNED; R : INTEGER) return BOOLEAN is
  begin
    if (L'length < 1) then
      assert NO_WARNING
        report "NUMERIC_BIT."">"": null argument detected, returning FALSE"
        severity warning;
      return false;
    end if;
    if SIGNED_NUM_BITS(R) > L'length then return 0 > R;
    end if;
    return not SIGNED_LESS_OR_EQUAL(L, TO_SIGNED(R, L'length));
  end function ">";

  -- ============================================================================

  -- Id: C.7
  function "<" (L, R : UNSIGNED) return BOOLEAN is
    constant SIZE : NATURAL := MAXIMUM(L'length, R'length);
  begin
    if ((L'length < 1) or (R'length < 1)) then
      assert NO_WARNING
        report "NUMERIC_BIT.""<"": null argument detected, returning FALSE"
        severity warning;
      return false;
    end if;
    return UNSIGNED_LESS(RESIZE(L, SIZE), RESIZE(R, SIZE));
  end function "<";

  -- Id: C.8
  function "<" (L, R : SIGNED) return BOOLEAN is
    constant SIZE : NATURAL := MAXIMUM(L'length, R'length);
  begin
    if ((L'length < 1) or (R'length < 1)) then
      assert NO_WARNING
        report "NUMERIC_BIT.""<"": null argument detected, returning FALSE"
        severity warning;
      return false;
    end if;
    return SIGNED_LESS(RESIZE(L, SIZE), RESIZE(R, SIZE));
  end function "<";

  -- Id: C.9
  function "<" (L : NATURAL; R : UNSIGNED) return BOOLEAN is
  begin
    if (R'length < 1) then
      assert NO_WARNING
        report "NUMERIC_BIT.""<"": null argument detected, returning FALSE"
        severity warning;
      return false;
    end if;
    if UNSIGNED_NUM_BITS(L) > R'length then return L < 0;
    end if;
    return UNSIGNED_LESS(TO_UNSIGNED(L, R'length), R);
  end function "<";

  -- Id: C.10
  function "<" (L : INTEGER; R : SIGNED) return BOOLEAN is
  begin
    if (R'length < 1) then
      assert NO_WARNING
        report "NUMERIC_BIT.""<"": null argument detected, returning FALSE"
        severity warning;
      return false;
    end if;
    if SIGNED_NUM_BITS(L) > R'length then return L < 0;
    end if;
    return SIGNED_LESS(TO_SIGNED(L, R'length), R);
  end function "<";

  -- Id: C.11
  function "<" (L : UNSIGNED; R : NATURAL) return BOOLEAN is
  begin
    if (L'length < 1) then
      assert NO_WARNING
        report "NUMERIC_BIT.""<"": null argument detected, returning FALSE"
        severity warning;
      return false;
    end if;
    if UNSIGNED_NUM_BITS(R) > L'length then return 0 < R;
    end if;
    return UNSIGNED_LESS(L, TO_UNSIGNED(R, L'length));
  end function "<";

  -- Id: C.12
  function "<" (L : SIGNED; R : INTEGER) return BOOLEAN is
  begin
    if (L'length < 1) then
      assert NO_WARNING
        report "NUMERIC_BIT.""<"": null argument detected, returning FALSE"
        severity warning;
      return false;
    end if;
    if SIGNED_NUM_BITS(R) > L'length then return 0 < R;
    end if;
    return SIGNED_LESS(L, TO_SIGNED(R, L'length));
  end function "<";

  -- ============================================================================

  -- Id: C.13
  function "<=" (L, R : UNSIGNED) return BOOLEAN is
    constant SIZE : NATURAL := MAXIMUM(L'length, R'length);
  begin
    if ((L'length < 1) or (R'length < 1)) then
      assert NO_WARNING
        report "NUMERIC_BIT.""<="": null argument detected, returning FALSE"
        severity warning;
      return false;
    end if;
    return UNSIGNED_LESS_OR_EQUAL(RESIZE(L, SIZE), RESIZE(R, SIZE));
  end function "<=";

  -- Id: C.14
  function "<=" (L, R : SIGNED) return BOOLEAN is
    constant SIZE : NATURAL := MAXIMUM(L'length, R'length);
  begin
    if ((L'length < 1) or (R'length < 1)) then
      assert NO_WARNING
        report "NUMERIC_BIT.""<="": null argument detected, returning FALSE"
        severity warning;
      return false;
    end if;
    return SIGNED_LESS_OR_EQUAL(RESIZE(L, SIZE), RESIZE(R, SIZE));
  end function "<=";

  -- Id: C.15
  function "<=" (L : NATURAL; R : UNSIGNED) return BOOLEAN is
  begin
    if (R'length < 1) then
      assert NO_WARNING
        report "NUMERIC_BIT.""<="": null argument detected, returning FALSE"
        severity warning;
      return false;
    end if;
    if UNSIGNED_NUM_BITS(L) > R'length then return L < 0;
    end if;
    return UNSIGNED_LESS_OR_EQUAL(TO_UNSIGNED(L, R'length), R);
  end function "<=";

  -- Id: C.16
  function "<=" (L : INTEGER; R : SIGNED) return BOOLEAN is
  begin
    if (R'length < 1) then
      assert NO_WARNING
        report "NUMERIC_BIT.""<="": null argument detected, returning FALSE"
        severity warning;
      return false;
    end if;
    if SIGNED_NUM_BITS(L) > R'length then return L < 0;
    end if;
    return SIGNED_LESS_OR_EQUAL(TO_SIGNED(L, R'length), R);
  end function "<=";

  -- Id: C.17
  function "<=" (L : UNSIGNED; R : NATURAL) return BOOLEAN is
  begin
    if (L'length < 1) then
      assert NO_WARNING
        report "NUMERIC_BIT.""<="": null argument detected, returning FALSE"
        severity warning;
      return false;
    end if;
    if UNSIGNED_NUM_BITS(R) > L'length then return 0 < R;
    end if;
    return UNSIGNED_LESS_OR_EQUAL(L, TO_UNSIGNED(R, L'length));
  end function "<=";

  -- Id: C.18
  function "<=" (L : SIGNED; R : INTEGER) return BOOLEAN is
  begin
    if (L'length < 1) then
      assert NO_WARNING
        report "NUMERIC_BIT.""<="": null argument detected, returning FALSE"
        severity warning;
      return false;
    end if;
    if SIGNED_NUM_BITS(R) > L'length then return 0 < R;
    end if;
    return SIGNED_LESS_OR_EQUAL(L, TO_SIGNED(R, L'length));
  end function "<=";

  -- ============================================================================

  -- Id: C.19
  function ">=" (L, R : UNSIGNED) return BOOLEAN is
    constant SIZE : NATURAL := MAXIMUM(L'length, R'length);
  begin
    if ((L'length < 1) or (R'length < 1)) then
      assert NO_WARNING
        report "NUMERIC_BIT."">="": null argument detected, returning FALSE"
        severity warning;
      return false;
    end if;
    return not UNSIGNED_LESS(RESIZE(L, SIZE), RESIZE(R, SIZE));
  end function ">=";

  -- Id: C.20
  function ">=" (L, R : SIGNED) return BOOLEAN is
    constant SIZE : NATURAL := MAXIMUM(L'length, R'length);
  begin
    if ((L'length < 1) or (R'length < 1)) then
      assert NO_WARNING
        report "NUMERIC_BIT."">="": null argument detected, returning FALSE"
        severity warning;
      return false;
    end if;
    return not SIGNED_LESS(RESIZE(L, SIZE), RESIZE(R, SIZE));
  end function ">=";

  -- Id: C.21
  function ">=" (L : NATURAL; R : UNSIGNED) return BOOLEAN is
  begin
    if (R'length < 1) then
      assert NO_WARNING
        report "NUMERIC_BIT."">="": null argument detected, returning FALSE"
        severity warning;
      return false;
    end if;
    if UNSIGNED_NUM_BITS(L) > R'length then return L > 0;
    end if;
    return not UNSIGNED_LESS(TO_UNSIGNED(L, R'length), R);
  end function ">=";

  -- Id: C.22
  function ">=" (L : INTEGER; R : SIGNED) return BOOLEAN is
  begin
    if (R'length < 1) then
      assert NO_WARNING
        report "NUMERIC_BIT."">="": null argument detected, returning FALSE"
        severity warning;
      return false;
    end if;
    if SIGNED_NUM_BITS(L) > R'length then return L > 0;
    end if;
    return not SIGNED_LESS(TO_SIGNED(L, R'length), R);
  end function ">=";

  -- Id: C.23
  function ">=" (L : UNSIGNED; R : NATURAL) return BOOLEAN is
  begin
    if (L'length < 1) then
      assert NO_WARNING
        report "NUMERIC_BIT."">="": null argument detected, returning FALSE"
        severity warning;
      return false;
    end if;
    if UNSIGNED_NUM_BITS(R) > L'length then return 0 > R;
    end if;
    return not UNSIGNED_LESS(L, TO_UNSIGNED(R, L'length));
  end function ">=";

  -- Id: C.24
  function ">=" (L : SIGNED; R : INTEGER) return BOOLEAN is
  begin
    if (L'length < 1) then
      assert NO_WARNING
        report "NUMERIC_BIT."">="": null argument detected, returning FALSE"
        severity warning;
      return false;
    end if;
    if SIGNED_NUM_BITS(R) > L'length then return 0 > R;
    end if;
    return not SIGNED_LESS(L, TO_SIGNED(R, L'length));
  end function ">=";

  -- ============================================================================

  -- Id: C.25
  function "=" (L, R : UNSIGNED) return BOOLEAN is
    constant SIZE : NATURAL := MAXIMUM(L'length, R'length);
  begin
    if ((L'length < 1) or (R'length < 1)) then
      assert NO_WARNING
        report "NUMERIC_BIT.""="": null argument detected, returning FALSE"
        severity warning;
      return false;
    end if;
    return UNSIGNED_EQUAL(RESIZE(L, SIZE), RESIZE(R, SIZE));
  end function "=";

  -- Id: C.26
  function "=" (L, R : SIGNED) return BOOLEAN is
    constant SIZE : NATURAL := MAXIMUM(L'length, R'length);
  begin
    if ((L'length < 1) or (R'length < 1)) then
      assert NO_WARNING
        report "NUMERIC_BIT.""="": null argument detected, returning FALSE"
        severity warning;
      return false;
    end if;
    return SIGNED_EQUAL(RESIZE(L, SIZE), RESIZE(R, SIZE));
  end function "=";

  -- Id: C.27
  function "=" (L : NATURAL; R : UNSIGNED) return BOOLEAN is
  begin
    if (R'length < 1) then
      assert NO_WARNING
        report "NUMERIC_BIT.""="": null argument detected, returning FALSE"
        severity warning;
      return false;
    end if;
    if UNSIGNED_NUM_BITS(L) > R'length then return false;
    end if;
    return UNSIGNED_EQUAL(TO_UNSIGNED(L, R'length), R);
  end function "=";

  -- Id: C.28
  function "=" (L : INTEGER; R : SIGNED) return BOOLEAN is
  begin
    if (R'length < 1) then
      assert NO_WARNING
        report "NUMERIC_BIT.""="": null argument detected, returning FALSE"
        severity warning;
      return false;
    end if;
    if SIGNED_NUM_BITS(L) > R'length then return false;
    end if;
    return SIGNED_EQUAL(TO_SIGNED(L, R'length), R);
  end function "=";

  -- Id: C.29
  function "=" (L : UNSIGNED; R : NATURAL) return BOOLEAN is
  begin
    if (L'length < 1) then
      assert NO_WARNING
        report "NUMERIC_BIT.""="": null argument detected, returning FALSE"
        severity warning;
      return false;
    end if;
    if UNSIGNED_NUM_BITS(R) > L'length then return false;
    end if;
    return UNSIGNED_EQUAL(L, TO_UNSIGNED(R, L'length));
  end function "=";

  -- Id: C.30
  function "=" (L : SIGNED; R : INTEGER) return BOOLEAN is
  begin
    if (L'length < 1) then
      assert NO_WARNING
        report "NUMERIC_BIT.""="": null argument detected, returning FALSE"
        severity warning;
      return false;
    end if;
    if SIGNED_NUM_BITS(R) > L'length then return false;
    end if;
    return SIGNED_EQUAL(L, TO_SIGNED(R, L'length));
  end function "=";

  -- ============================================================================

  -- Id: C.31
  function "/=" (L, R : UNSIGNED) return BOOLEAN is
    constant SIZE : NATURAL := MAXIMUM(L'length, R'length);
  begin
    if ((L'length < 1) or (R'length < 1)) then
      assert NO_WARNING
        report "NUMERIC_BIT.""/="": null argument detected, returning TRUE"
        severity warning;
      return true;
    end if;
    return not(UNSIGNED_EQUAL(RESIZE(L, SIZE), RESIZE(R, SIZE)));
  end function "/=";

  -- Id: C.32
  function "/=" (L, R : SIGNED) return BOOLEAN is
    constant SIZE : NATURAL := MAXIMUM(L'length, R'length);
  begin
    if ((L'length < 1) or (R'length < 1)) then
      assert NO_WARNING
        report "NUMERIC_BIT.""/="": null argument detected, returning TRUE"
        severity warning;
      return true;
    end if;
    return not(SIGNED_EQUAL(RESIZE(L, SIZE), RESIZE(R, SIZE)));
  end function "/=";

  -- Id: C.33
  function "/=" (L : NATURAL; R : UNSIGNED) return BOOLEAN is
  begin
    if (R'length < 1) then
      assert NO_WARNING
        report "NUMERIC_BIT.""/="": null argument detected, returning TRUE"
        severity warning;
      return true;
    end if;
    if UNSIGNED_NUM_BITS(L) > R'length then return true;
    end if;
    return not(UNSIGNED_EQUAL(TO_UNSIGNED(L, R'length), R));
  end function "/=";

  -- Id: C.34
  function "/=" (L : INTEGER; R : SIGNED) return BOOLEAN is
  begin
    if (R'length < 1) then
      assert NO_WARNING
        report "NUMERIC_BIT.""/="": null argument detected, returning TRUE"
        severity warning;
      return true;
    end if;
    if SIGNED_NUM_BITS(L) > R'length then return true;
    end if;
    return not(SIGNED_EQUAL(TO_SIGNED(L, R'length), R));
  end function "/=";

  -- Id: C.35
  function "/=" (L : UNSIGNED; R : NATURAL) return BOOLEAN is
  begin
    if (L'length < 1) then
      assert NO_WARNING
        report "NUMERIC_BIT.""/="": null argument detected, returning TRUE"
        severity warning;
      return true;
    end if;
    if UNSIGNED_NUM_BITS(R) > L'length then return true;
    end if;
    return not(UNSIGNED_EQUAL(L, TO_UNSIGNED(R, L'length)));
  end function "/=";

  -- Id: C.36
  function "/=" (L : SIGNED; R : INTEGER) return BOOLEAN is
  begin
    if (L'length < 1) then
      assert NO_WARNING
        report "NUMERIC_BIT.""/="": null argument detected, returning TRUE"
        severity warning;
      return true;
    end if;
    if SIGNED_NUM_BITS(R) > L'length then return true;
    end if;
    return not(SIGNED_EQUAL(L, TO_SIGNED(R, L'length)));
  end function "/=";

  -- ============================================================================

  -- Id: S.1
  function SHIFT_LEFT (ARG : UNSIGNED; COUNT : NATURAL) return UNSIGNED is
  begin
    if (ARG'length < 1) then return NAU;
    end if;
    return UNSIGNED(XSLL(BIT_VECTOR(ARG), COUNT));
  end function SHIFT_LEFT;

  -- Id: S.2
  function SHIFT_RIGHT (ARG : UNSIGNED; COUNT : NATURAL) return UNSIGNED is
  begin
    if (ARG'length < 1) then return NAU;
    end if;
    return UNSIGNED(XSRL(BIT_VECTOR(ARG), COUNT));
  end function SHIFT_RIGHT;

  -- Id: S.3
  function SHIFT_LEFT (ARG : SIGNED; COUNT : NATURAL) return SIGNED is
  begin
    if (ARG'length < 1) then return NAS;
    end if;
    return SIGNED(XSLL(BIT_VECTOR(ARG), COUNT));
  end function SHIFT_LEFT;

  -- Id: S.4
  function SHIFT_RIGHT (ARG : SIGNED; COUNT : NATURAL) return SIGNED is
  begin
    if (ARG'length < 1) then return NAS;
    end if;
    return SIGNED(XSRA(BIT_VECTOR(ARG), COUNT));
  end function SHIFT_RIGHT;

  -- ============================================================================

  -- Id: S.5
  function ROTATE_LEFT (ARG : UNSIGNED; COUNT : NATURAL) return UNSIGNED is
  begin
    if (ARG'length < 1) then return NAU;
    end if;
    return UNSIGNED(XROL(BIT_VECTOR(ARG), COUNT));
  end function ROTATE_LEFT;

  -- Id: S.6
  function ROTATE_RIGHT (ARG : UNSIGNED; COUNT : NATURAL) return UNSIGNED is
  begin
    if (ARG'length < 1) then return NAU;
    end if;
    return UNSIGNED(XROR(BIT_VECTOR(ARG), COUNT));
  end function ROTATE_RIGHT;

  -- Id: S.7
  function ROTATE_LEFT (ARG : SIGNED; COUNT : NATURAL) return SIGNED is
  begin
    if (ARG'length < 1) then return NAS;
    end if;
    return SIGNED(XROL(BIT_VECTOR(ARG), COUNT));
  end function ROTATE_LEFT;

  -- Id: S.8
  function ROTATE_RIGHT (ARG : SIGNED; COUNT : NATURAL) return SIGNED is
  begin
    if (ARG'length < 1) then return NAS;
    end if;
    return SIGNED(XROR(BIT_VECTOR(ARG), COUNT));
  end function ROTATE_RIGHT;

  -- ============================================================================

  ------------------------------------------------------------------------------
  -- Note: Function S.9 is not compatible with IEEE Std 1076-1987. Comment
  -- out the function (declaration and body) for IEEE Std 1076-1987 compatibility.
  ------------------------------------------------------------------------------
  -- Id: S.9
  function "sll" (ARG : UNSIGNED; COUNT : INTEGER) return UNSIGNED is
  begin
    if (COUNT >= 0) then
      return SHIFT_LEFT(ARG, COUNT);
    else
      return SHIFT_RIGHT(ARG, -COUNT);
    end if;
  end function "sll";

  ------------------------------------------------------------------------------
  -- Note: Function S.10 is not compatible with IEEE Std 1076-1987. Comment
  -- out the function (declaration and body) for IEEE Std 1076-1987 compatibility.
  ------------------------------------------------------------------------------
  -- Id: S.10
  function "sll" (ARG : SIGNED; COUNT : INTEGER) return SIGNED is
  begin
    if (COUNT >= 0) then
      return SHIFT_LEFT(ARG, COUNT);
    else
      return SIGNED(SHIFT_RIGHT(UNSIGNED(ARG), -COUNT));
    end if;
  end function "sll";

  ------------------------------------------------------------------------------
  -- Note: Function S.11 is not compatible with IEEE Std 1076-1987. Comment
  -- out the function (declaration and body) for IEEE Std 1076-1987 compatibility.
  ------------------------------------------------------------------------------
  -- Id: S.11
  function "srl" (ARG : UNSIGNED; COUNT : INTEGER) return UNSIGNED is
  begin
    if (COUNT >= 0) then
      return SHIFT_RIGHT(ARG, COUNT);
    else
      return SHIFT_LEFT(ARG, -COUNT);
    end if;
  end function "srl";

  ------------------------------------------------------------------------------
  -- Note: Function S.12 is not compatible with IEEE Std 1076-1987. Comment
  -- out the function (declaration and body) for IEEE Std 1076-1987 compatibility.
  ------------------------------------------------------------------------------
  -- Id: S.12
  function "srl" (ARG : SIGNED; COUNT : INTEGER) return SIGNED is
  begin
    if (COUNT >= 0) then
      return SIGNED(SHIFT_RIGHT(UNSIGNED(ARG), COUNT));
    else
      return SHIFT_LEFT(ARG, -COUNT);
    end if;
  end function "srl";

  ------------------------------------------------------------------------------
  -- Note: Function S.13 is not compatible with IEEE Std 1076-1987. Comment
  -- out the function (declaration and body) for IEEE Std 1076-1987 compatibility.
  ------------------------------------------------------------------------------
  -- Id: S.13
  function "rol" (ARG : UNSIGNED; COUNT : INTEGER) return UNSIGNED is
  begin
    if (COUNT >= 0) then
      return ROTATE_LEFT(ARG, COUNT);
    else
      return ROTATE_RIGHT(ARG, -COUNT);
    end if;
  end function "rol";

  ------------------------------------------------------------------------------
  -- Note: Function S.14 is not compatible with IEEE Std 1076-1987. Comment
  -- out the function (declaration and body) for IEEE Std 1076-1987 compatibility.
  ------------------------------------------------------------------------------
  -- Id: S.14
  function "rol" (ARG : SIGNED; COUNT : INTEGER) return SIGNED is
  begin
    if (COUNT >= 0) then
      return ROTATE_LEFT(ARG, COUNT);
    else
      return ROTATE_RIGHT(ARG, -COUNT);
    end if;
  end function "rol";

  ------------------------------------------------------------------------------
  -- Note: Function S.15 is not compatible with IEEE Std 1076-1987. Comment
  -- out the function (declaration and body) for IEEE Std 1076-1987 compatibility.
  ------------------------------------------------------------------------------
  -- Id: S.15
  function "ror" (ARG : UNSIGNED; COUNT : INTEGER) return UNSIGNED is
  begin
    if (COUNT >= 0) then
      return ROTATE_RIGHT(ARG, COUNT);
    else
      return ROTATE_LEFT(ARG, -COUNT);
    end if;
  end function "ror";

  ------------------------------------------------------------------------------
  -- Note: Function S.16 is not compatible with IEEE Std 1076-1987. Comment
  -- out the function (declaration and body) for IEEE Std 1076-1987 compatibility.
  ------------------------------------------------------------------------------
  -- Id: S.16
  function "ror" (ARG : SIGNED; COUNT : INTEGER) return SIGNED is
  begin
    if (COUNT >= 0) then
      return ROTATE_RIGHT(ARG, COUNT);
    else
      return ROTATE_LEFT(ARG, -COUNT);
    end if;
  end function "ror";

  -- ============================================================================

  -- Id: D.1
  function TO_INTEGER (ARG : UNSIGNED) return NATURAL is
    constant ARG_LEFT : INTEGER := ARG'length-1;
    alias XARG        : UNSIGNED(ARG_LEFT downto 0) is ARG;
    variable RESULT   : NATURAL := 0;
  begin
    if (ARG'length < 1) then
      assert NO_WARNING
        report "NUMERIC_BIT.TO_INTEGER: null detected, returning 0"
        severity warning;
      return 0;
    end if;
    for I in XARG'range loop
      RESULT := RESULT+RESULT;
      if XARG(I) = '1' then
        RESULT := RESULT + 1;
      end if;
    end loop;
    return RESULT;
  end function TO_INTEGER;

  -- Id: D.2
  function TO_INTEGER (ARG : SIGNED) return INTEGER is
  begin
    if (ARG'length < 1) then
      assert NO_WARNING
        report "NUMERIC_BIT.TO_INTEGER: null detected, returning 0"
        severity warning;
      return 0;
    end if;
    if ARG(ARG'left) = '0' then
      return TO_INTEGER(UNSIGNED(ARG));
    else
      return (- (TO_INTEGER(UNSIGNED(- (ARG + 1)))) -1);
    end if;
  end function TO_INTEGER;

  -- Id: D.3
  function TO_UNSIGNED (ARG, SIZE : NATURAL) return UNSIGNED is
    variable RESULT : UNSIGNED(SIZE-1 downto 0);
    variable I_VAL  : NATURAL := ARG;
  begin
    if (SIZE < 1) then return NAU;
    end if;
    for I in 0 to RESULT'left loop
      if (I_VAL mod 2) = 0 then
        RESULT(I) := '0';
      else RESULT(I) := '1';
      end if;
      I_VAL          := I_VAL/2;
    end loop;
    if not(I_VAL = 0) then
      assert NO_WARNING
        report "NUMERIC_BIT.TO_UNSIGNED: vector truncated"
        severity warning;
    end if;
    return RESULT;
  end function TO_UNSIGNED;

  -- Id: D.4
  function TO_SIGNED (ARG  : INTEGER;
                      SIZE : NATURAL) return SIGNED is
    variable RESULT : SIGNED(SIZE-1 downto 0);
    variable B_VAL  : BIT     := '0';
    variable I_VAL  : INTEGER := ARG;
  begin
    if (SIZE < 1) then return NAS;
    end if;
    if (ARG < 0) then
      B_VAL := '1';
      I_VAL := -(ARG+1);
    end if;
    for I in 0 to RESULT'left loop
      if (I_VAL mod 2) = 0 then
        RESULT(I) := B_VAL;
      else
        RESULT(I) := not B_VAL;
      end if;
      I_VAL := I_VAL/2;
    end loop;
    if ((I_VAL /= 0) or (B_VAL /= RESULT(RESULT'left))) then
      assert NO_WARNING
        report "NUMERIC_BIT.TO_SIGNED: vector truncated"
        severity warning;
    end if;
    return RESULT;
  end function TO_SIGNED;

  -- ============================================================================

  -- Id: R.1
  function RESIZE (ARG : SIGNED; NEW_SIZE : NATURAL) return SIGNED is
    alias INVEC     : SIGNED(ARG'length-1 downto 0) is ARG;
    variable RESULT : SIGNED(NEW_SIZE-1 downto 0) := (others => '0');
    constant BOUND  : INTEGER                     := MINIMUM(ARG'length, RESULT'length)-2;
  begin
    if (NEW_SIZE < 1) then return NAS;
    end if;
    if (ARG'length = 0) then return RESULT;
    end if;
    RESULT := (others => ARG(ARG'left));
    if BOUND >= 0 then
      RESULT(BOUND downto 0) := INVEC(BOUND downto 0);
    end if;
    return RESULT;
  end function RESIZE;

  -- Id: R.2
  function RESIZE (ARG : UNSIGNED; NEW_SIZE : NATURAL) return UNSIGNED is
    constant ARG_LEFT : INTEGER                       := ARG'length-1;
    alias XARG        : UNSIGNED(ARG_LEFT downto 0) is ARG;
    variable RESULT   : UNSIGNED(NEW_SIZE-1 downto 0) := (others => '0');
  begin
    if (NEW_SIZE < 1) then return NAU;
    end if;
    if XARG'length = 0 then return RESULT;
    end if;
    if (RESULT'length < ARG'length) then
      RESULT(RESULT'left downto 0) := XARG(RESULT'left downto 0);
    else
      RESULT(RESULT'left downto XARG'left+1) := (others => '0');
      RESULT(XARG'left downto 0)             := XARG;
    end if;
    return RESULT;
  end function RESIZE;

  function RESIZE (ARG, SIZE_RES : UNSIGNED)
    return UNSIGNED is
  begin
    return RESIZE (ARG      => ARG,
                   NEW_SIZE => SIZE_RES'length);
  end function RESIZE;

  function RESIZE (ARG, SIZE_RES : SIGNED)
    return SIGNED is
  begin
    return RESIZE (ARG      => ARG,
                   NEW_SIZE => SIZE_RES'length);
  end function RESIZE;

  -- ============================================================================

  -- Id: L.1
  function "not" (L : UNSIGNED) return UNSIGNED is
    variable RESULT : UNSIGNED(L'length-1 downto 0);
  begin
    RESULT := UNSIGNED(not(BIT_VECTOR(L)));
    return RESULT;
  end function "not";

  -- Id: L.2
  function "and" (L, R : UNSIGNED) return UNSIGNED is
    variable RESULT : UNSIGNED(L'length-1 downto 0);
  begin
    RESULT := UNSIGNED(BIT_VECTOR(L) and BIT_VECTOR(R));
    return RESULT;
  end function "and";

  -- Id: L.3
  function "or" (L, R : UNSIGNED) return UNSIGNED is
    variable RESULT : UNSIGNED(L'length-1 downto 0);
  begin
    RESULT := UNSIGNED(BIT_VECTOR(L) or BIT_VECTOR(R));
    return RESULT;
  end function "or";

  -- Id: L.4
  function "nand" (L, R : UNSIGNED) return UNSIGNED is
    variable RESULT : UNSIGNED(L'length-1 downto 0);
  begin
    RESULT := UNSIGNED(BIT_VECTOR(L) nand BIT_VECTOR(R));
    return RESULT;
  end function "nand";

  -- Id: L.5
  function "nor" (L, R : UNSIGNED) return UNSIGNED is
    variable RESULT : UNSIGNED(L'length-1 downto 0);
  begin
    RESULT := UNSIGNED(BIT_VECTOR(L) nor BIT_VECTOR(R));
    return RESULT;
  end function "nor";

  -- Id: L.6
  function "xor" (L, R : UNSIGNED) return UNSIGNED is
    variable RESULT : UNSIGNED(L'length-1 downto 0);
  begin
    RESULT := UNSIGNED(BIT_VECTOR(L) xor BIT_VECTOR(R));
    return RESULT;
  end function "xor";

  ------------------------------------------------------------------------------
  -- Note: Function L.7 is not compatible with IEEE Std 1076-1987. Comment
  -- out the function (declaration and body) for IEEE Std 1076-1987 compatibility.
  ------------------------------------------------------------------------------
  -- Id: L.7
  function "xnor" (L, R : UNSIGNED) return UNSIGNED is
    variable RESULT : UNSIGNED(L'length-1 downto 0);
  begin
    RESULT := UNSIGNED(BIT_VECTOR(L) xnor BIT_VECTOR(R));
    return RESULT;
  end function "xnor";

  -- Id: L.8
  function "not" (L : SIGNED) return SIGNED is
    variable RESULT : SIGNED(L'length-1 downto 0);
  begin
    RESULT := SIGNED(not(BIT_VECTOR(L)));
    return RESULT;
  end function "not";

  -- Id: L.9
  function "and" (L, R : SIGNED) return SIGNED is
    variable RESULT : SIGNED(L'length-1 downto 0);
  begin
    RESULT := SIGNED(BIT_VECTOR(L) and BIT_VECTOR(R));
    return RESULT;
  end function "and";

  -- Id: L.10
  function "or" (L, R : SIGNED) return SIGNED is
    variable RESULT : SIGNED(L'length-1 downto 0);
  begin
    RESULT := SIGNED(BIT_VECTOR(L) or BIT_VECTOR(R));
    return RESULT;
  end function "or";

  -- Id: L.11
  function "nand" (L, R : SIGNED) return SIGNED is
    variable RESULT : SIGNED(L'length-1 downto 0);
  begin
    RESULT := SIGNED(BIT_VECTOR(L) nand BIT_VECTOR(R));
    return RESULT;
  end function "nand";

  -- Id: L.12
  function "nor" (L, R : SIGNED) return SIGNED is
    variable RESULT : SIGNED(L'length-1 downto 0);
  begin
    RESULT := SIGNED(BIT_VECTOR(L) nor BIT_VECTOR(R));
    return RESULT;
  end function "nor";

  -- Id: L.13
  function "xor" (L, R : SIGNED) return SIGNED is
    variable RESULT : SIGNED(L'length-1 downto 0);
  begin
    RESULT := SIGNED(BIT_VECTOR(L) xor BIT_VECTOR(R));
    return RESULT;
  end function "xor";

  ------------------------------------------------------------------------------
  -- Note: Function L.14 is not compatible with IEEE Std 1076-1987. Comment
  -- out the function (declaration and body) for IEEE Std 1076-1987 compatibility.
  ------------------------------------------------------------------------------
  -- Id: L.14
  function "xnor" (L, R : SIGNED) return SIGNED is
    variable RESULT : SIGNED(L'length-1 downto 0);
  begin
    RESULT := SIGNED(BIT_VECTOR(L) xnor BIT_VECTOR(R));
    return RESULT;
  end function "xnor";

  --============================================================================

  -- Id: E.1
  function RISING_EDGE (signal S: BIT) return BOOLEAN is
  begin
    return S'EVENT and S = '1';
  end RISING_EDGE;

  -- Id: E.2
  function FALLING_EDGE (signal S: BIT) return BOOLEAN is
  begin
    return S'EVENT and S = '0';
  end FALLING_EDGE;

end package body NUMERIC_BIT;

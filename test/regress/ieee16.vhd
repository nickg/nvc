------------------------------------------------------------------------
-- Copyright 1996 by VHDL International. All rights reserved.
--
-- This source file is considered to be an aid to check implementations of the
-- the IEEE standard 1076.2 and may be distributed without restriction
-- as long as it is not sold or distributed for profit.
--

-- THIS SOFTWARE IS PROVIDED BY CONTRIBUTORS ``AS IS'' AND ANY EXPRESS OR
-- IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
-- WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
-- DISCLAIMED. IN NO EVENT SHALL CONTRIBUTORS, IEEE, OR VHDL
-- INTERNATIONAL BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
-- EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
-- PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
-- PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
-- LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
-- NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
-- SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
--
-- Title:       Testbench for Standard VHDL MATH_REAL Package (PAR 1076.2)
--
-- Developers:  Members of the VHDL Mathematical Packages Working Group
--              Code was written by Charles Swart (cswart@analogy.com)
--              Data was supplied by many people.
--
-- Purpose:     An aid to check implementations of the IEEE MATH_REAL
--              package, Standard 1076.2.
--
--
-- Library:     This  can be compiled into any convient library.
--
-- Limitation:  Only values in the minimum required range may be tested
--              to keep this test portable.
--              That range is -1.0E38 to +1.0E38 inclusive.
--
-- Notes:
--              This file consists of an entity REAL_TESTS and a single
--              architecture, ARCH. It references packages MATH_REAL
--              and TEXTIO.
--
--              To run this set of tests, choose an appropriate value for
--              the constant FULL_RESULTS, compile and simulate for
--              about 100 ns.
--
--              Each process begins with a WAIT for a unique time interval.
--              The purpose of this is to impose an order of evaluation
--              on the tests to ease data comparisons between different
--              implementations.
--
--              The data was optained from a number of sources and is not
--              guaranteed to be accurate. Many results are highly sensitive
--              on the character to real conversion, which is often
--              inaccurate in the last few places.
--
--              Also, the tests are not comprehensive.
--
--              The tests report the number of decimal digits of accuracy.
--              There are several problems with this measurement.
--              In some cases the number reported is one too many.
--              The measurement also, probably correctly, reports that
--              5.0 and 4.99999 are close.
--              A more fundamental problem involves values near zero.
--              The test can (correctly) report 0 digits accuracy for small
--              values.
--              However, the results might still be perfectly reasonable.
--
---             Suggestions, improvements and additional test points
--              are welcome.
--
-- -------------------------------------------------------------------------
-- Modification history:
-- -------------------------------------------------------------------------
--      v0.91    | 05/01/96    | pre-release version
-- Modified for VHDL-2006 05/25/06 David Bishop (dbishop@vhdl.org)
-------------------------------------------------------------

library ieee;
use ieee.math_real.all;
use std.textio.all;
entity IEEE16 is
  generic (
    quiet : BOOLEAN := false;          -- run quietly
    FULL_RESULTS : BOOLEAN := false);   -- verbose output
end entity IEEE16;
architecture ARCH of IEEE16 is

  type REALX_VECTOR is array(POSITIVE range <>) of REAL;
  type INT_VECTOR is array(POSITIVE range <>) of INTEGER;
--  constant FULL_RESULTS : BOOLEAN := false;  -- TRUE for full results, FALSE for summary
  constant MAX_DIGITS   : INTEGER := 60;  -- large value to represent exact agreement
  constant NEAR_ZERO    : REAL    := 1.0E-07;  -- values less than this are considered zero

  function COMPARE(EXPECTED : REAL; RESULT : REAL) return INTEGER is

    variable DENOMINATOR : REAL;
    variable TEMP1       : REAL;
    variable EXPECTED1   : REAL := EXPECTED;

  begin
    if EXPECTED1 = RESULT then return MAX_DIGITS; end if;

    --This is a kludge to get more reasonable results for values near zero.
    --The basic problem is that small representational errors may cause the
    --computed result to be more accurate than the "correct" expected result

    if ((abs(EXPECTED1) < NEAR_ZERO) and (abs(RESULT-0.0) < abs(RESULT-EXPECTED1)))
    then EXPECTED1 := 0.0;
    end if;

    if EXPECTED1 = 0.0
    then DENOMINATOR := 1.0;
    else DENOMINATOR := 10.0 ** (INTEGER(FLOOR(LOG10(abs(EXPECTED1)))));
    end if;

    TEMP1 := LOG10(abs((EXPECTED1-RESULT)/DENOMINATOR));

    if TEMP1 > 0.0 then return 0; end if;

    return INTEGER(CEIL(-TEMP1));

  end COMPARE;

  procedure PRINT_RESULTS(FUNC_NAME       : STRING; ARG1 : REALX_VECTOR; RESULTS : REALX_VECTOR;
                          CORRECT_ANSWERS : REALX_VECTOR;
                          ACCURACY        : INT_VECTOR;
                          FULL_RESULTS    : BOOLEAN) is
    variable WORST_ACCURACY : INTEGER := ACCURACY(1);
    variable OUTLINE        : LINE;

  begin
    if(FULL_RESULTS) then

      --write out header
      WRITE(OUTLINE, FUNC_NAME, left, 10);
      WRITE(OUTLINE, STRING'("argument"), left, 25);
      WRITE(OUTLINE, STRING'("result"), left, 25);
      WRITE(OUTLINE, STRING'("correct"), left, 25);
      WRITE(OUTLINE, STRING'("digits"));
      WRITELINE(OUTPUT, OUTLINE);

      --write out each value
      for I in 1 to ARG1'length loop
        WRITE(OUTLINE, STRING'("          "));
        WRITE(OUTLINE, ARG1(I), left, 25);
        WRITE(OUTLINE, RESULTS(I), left, 25);
        WRITE(OUTLINE, CORRECT_ANSWERS(I), left, 25);
        WRITE(OUTLINE, ACCURACY(I), right, 2);
        WRITELINE(OUTPUT, OUTLINE);
      end loop;

    end if;

    --update accuracy
    for I in 1 to ACCURACY'length loop
      if ACCURACY(I) < WORST_ACCURACY then WORST_ACCURACY := ACCURACY(I); end if;
    end loop;

    if not quiet or WORST_ACCURACY < 6 then
      --in all cases print summary information
      WRITE(OUTLINE, FUNC_NAME);
      WRITE(OUTLINE, STRING'(" has worst case accuracy of "));
      WRITE(OUTLINE, WORST_ACCURACY);
      WRITE(OUTLINE, STRING'(" digits."));
      WRITELINE(OUTPUT, OUTLINE);
      --print blank line
      WRITELINE(OUTPUT, OUTLINE);
    end if;

    assert WORST_ACCURACY >= 6;

  end PRINT_RESULTS;

  procedure PRINT_RESULTS(FUNC_NAME       : STRING; ARG1 : REALX_VECTOR; ARG2 : REALX_VECTOR; RESULTS : REALX_VECTOR;
                          CORRECT_ANSWERS : REALX_VECTOR;
                          ACCURACY        : INT_VECTOR;
                          FULL_RESULTS    : BOOLEAN) is
    variable WORST_ACCURACY : INTEGER := ACCURACY(1);
    variable OUTLINE        : LINE;

  begin
    if(FULL_RESULTS) then

      --write out header
      WRITE(OUTLINE, FUNC_NAME, left, 10);
      WRITE(OUTLINE, STRING'("argument1"), left, 25);
      WRITE(OUTLINE, STRING'("argument2"), left, 25);
      WRITE(OUTLINE, STRING'("result"), left, 25);
      WRITE(OUTLINE, STRING'("correct"), left, 25);
      WRITE(OUTLINE, STRING'("digits"));
      WRITELINE(OUTPUT, OUTLINE);

      --write out each value
      for I in 1 to ARG1'length loop
        WRITE(OUTLINE, STRING'("          "));
        WRITE(OUTLINE, ARG1(I), left, 25);
        WRITE(OUTLINE, ARG2(I), left, 25);
        WRITE(OUTLINE, RESULTS(I), left, 25);
        WRITE(OUTLINE, CORRECT_ANSWERS(I), left, 25);
        WRITE(OUTLINE, ACCURACY(I), right, 2);
        WRITELINE(OUTPUT, OUTLINE);
      end loop;

    end if;

    --update accuracy
    for I in 1 to ACCURACY'length loop
      if ACCURACY(I) < WORST_ACCURACY then WORST_ACCURACY := ACCURACY(I); end if;
    end loop;

    if not quiet or WORST_ACCURACY < 6 then
      --in all cases print summary information
      WRITE(OUTLINE, FUNC_NAME);
      WRITE(OUTLINE, STRING'(" has worst case accuracy of "));
      WRITE(OUTLINE, WORST_ACCURACY);
      WRITE(OUTLINE, STRING'(" digits."));
      WRITELINE(OUTPUT, OUTLINE);
      --print blank line
      WRITELINE(OUTPUT, OUTLINE);
    end if;

    assert WORST_ACCURACY >= 6;

  end PRINT_RESULTS;

--      print results for uniform procedure
  procedure PRINT_RESULTS(SEED1            : INT_VECTOR; SEED2 : INT_VECTOR;
                          NEW_SEED1        : INT_VECTOR; NEW_SEED2 : INT_VECTOR;
                          RESULTS          : REALX_VECTOR;
                          EXPECTED_SEED1   : INT_VECTOR; EXPECTED_SEED2 : INT_VECTOR;
                          EXPECTED_RESULTS : REALX_VECTOR;
                          FULL_RESULTS     : BOOLEAN) is

    variable OUTLINE    : LINE;
    variable FAIL_COUNT : INTEGER := 0;

  begin

    if(FULL_RESULTS) then

      --write out header
      WRITE(OUTLINE, STRING'("Test of UNIFORM procedure."));
      WRITELINE(OUTPUT, OUTLINE);
      WRITE(OUTLINE, STRING'("seed1"), left, 12);
      WRITE(OUTLINE, STRING'("seed2"), left, 12);
      WRITE(OUTLINE, STRING'("new seed1"), left, 12);
      WRITE(OUTLINE, STRING'("new seed2"), left, 12);
      WRITE(OUTLINE, STRING'("results"), left, 21);
      WRITE(OUTLINE, STRING'("expected seed1"), left, 16);
      WRITE(OUTLINE, STRING'("expected seed2"), left, 16);
      WRITE(OUTLINE, STRING'("expected results"), left, 25);
      WRITELINE(OUTPUT, OUTLINE);

      --write out each value
      for I in 1 to SEED1'length loop
        WRITE(OUTLINE, SEED1(I), left, 12);
        WRITE(OUTLINE, SEED2(I), left, 12);
        WRITE(OUTLINE, NEW_SEED1(I), left, 12);
        WRITE(OUTLINE, NEW_SEED2(I), left, 12);
        WRITE(OUTLINE, RESULTS(I), left, 23);
        WRITE(OUTLINE, EXPECTED_SEED1(I), left, 12);
        WRITE(OUTLINE, EXPECTED_SEED2(I), left, 12);
        WRITE(OUTLINE, EXPECTED_RESULTS(I), left, 23);
        WRITELINE(OUTPUT, OUTLINE);

        if((NEW_SEED1(I) /= EXPECTED_SEED1(I)) or (NEW_SEED2(I) /= EXPECTED_SEED2(I))) then
          FAIL_COUNT := FAIL_COUNT+1;
        end if;

      end loop;
    end if;

    if FAIL_COUNT /= 0 or not quiet then
      --In all cases print summary information
      WRITE(OUTLINE, STRING'(" Procedure UNIFORM produced incorrect seeds in "));
      WRITE(OUTLINE, FAIL_COUNT);
      WRITE(OUTLINE, STRING'(" cases."));
      WRITELINE(OUTPUT, OUTLINE);
      --print blank line
      WRITELINE(OUTPUT, OUTLINE);
    end if;

    assert FAIL_COUNT = 0;

  end PRINT_RESULTS;

begin

  SIGN_TEST : process

    constant ARG1 : REALX_VECTOR := (

      0.0,
      1.0,
      -1.0,
      5.0,
      -5.0,
      4.3,
      4.7,
      4.5,
      -4.3,
      -4.7,
      -4.5,
      -1.0E38,
      1.0E38);


    constant CORRECT_ANSWERS : REALX_VECTOR(1 to ARG1'length) := (

      0.0,
      1.0,
      -1.0,
      1.0,
      -1.0,
      1.0,
      1.0,
      1.0,
      -1.0,
      -1.0,
      -1.0,
      -1.0,
      1.0);

    variable RESULTS  : REALX_VECTOR(1 to ARG1'length);
    variable ACCURACY : INT_VECTOR(1 to ARG1'length);

  begin

    wait for 1 ns;

--      compute results

    for I in 1 to ARG1'length loop
      RESULTS(I) := SIGN(ARG1(I));
    end loop;

--      compute accuracy

    for I in 1 to ARG1'length loop
      ACCURACY(I) := COMPARE(CORRECT_ANSWERS(I), RESULTS(I));
    end loop;

--      print results

    PRINT_RESULTS("SIGN", ARG1, RESULTS, CORRECT_ANSWERS, ACCURACY, FULL_RESULTS);

    wait;

  end process SIGN_TEST;

  CEIL_TEST : process

    constant ARG1 : REALX_VECTOR := (

      0.0,
      1.0,
      -1.0,
      5.0,
      -5.0,
      4.3,
      4.7,
      4.5,
      -4.3,
      -4.7,
      -4.5,
      -1.0E38,
      1.0E38);


    constant CORRECT_ANSWERS : REALX_VECTOR(1 to ARG1'length) := (

      0.0,
      1.0,
      -1.0,
      5.0,
      -5.0,
      5.0,
      5.0,
      5.0,
      -4.0,
      -4.0,
      -4.0,
      -1.0E38,
      1.0E38);

    variable RESULTS  : REALX_VECTOR(1 to ARG1'length);
    variable ACCURACY : INT_VECTOR(1 to ARG1'length);

  begin

    wait for 2 ns;

--      compute results

    for I in 1 to ARG1'length loop
      RESULTS(I) := CEIL(ARG1(I));
    end loop;

--      compute accuracy

    for I in 1 to ARG1'length loop
      ACCURACY(I) := COMPARE(CORRECT_ANSWERS(I), RESULTS(I));
    end loop;

--      print results

    PRINT_RESULTS("CEIL", ARG1, RESULTS, CORRECT_ANSWERS, ACCURACY, FULL_RESULTS);

    wait;

  end process CEIL_TEST;

  FLOOR_TEST : process

    constant ARG1 : REALX_VECTOR := (

      0.0,
      1.0,
      -1.0,
      5.0,
      -5.0,
      4.3,
      4.7,
      4.5,
      -4.3,
      -4.7,
      -4.5,
      128.0,
      -128.0,
      -1.0E38,
      1.0E38);

    constant CORRECT_ANSWERS : REALX_VECTOR(1 to ARG1'length) := (

      0.0,
      1.0,
      -1.0,
      5.0,
      -5.0,
      4.0,
      4.0,
      4.0,
      -5.0,
      -5.0,
      -5.0,
      128.0,
      -128.0,
      -1.0E38,
      1.0E38);

    variable RESULTS  : REALX_VECTOR(1 to ARG1'length);
    variable ACCURACY : INT_VECTOR(1 to ARG1'length);

  begin

    wait for 3 ns;

--      compute results

    for I in 1 to ARG1'length loop
      RESULTS(I) := FLOOR(ARG1(I));
    end loop;

--      compute accuracy

    for I in 1 to ARG1'length loop
      ACCURACY(I) := COMPARE(CORRECT_ANSWERS(I), RESULTS(I));
    end loop;

--      print results

    PRINT_RESULTS("FLOOR", ARG1, RESULTS, CORRECT_ANSWERS, ACCURACY, FULL_RESULTS);

    wait;

  end process FLOOR_TEST;

  ROUND_TEST : process

    constant ARG1 : REALX_VECTOR := (

      0.0,
      1.0,
      -1.0,
      5.0,
      -5.0,
      4.3,
      4.7,
      4.5,
      -4.3,
      -4.7,
      -4.5,
      -1.0E38,
      1.0E38);


    constant CORRECT_ANSWERS : REALX_VECTOR(1 to ARG1'length) := (

      0.0,
      1.0,
      -1.0,
      5.0,
      -5.0,
      4.0,
      5.0,
      5.0,
      -4.0,
      -5.0,
      -5.0,
      -1.0E38,
      1.0E38);

    variable RESULTS  : REALX_VECTOR(1 to ARG1'length);
    variable ACCURACY : INT_VECTOR(1 to ARG1'length);

  begin

    wait for 4 ns;

--      compute results

    for I in 1 to ARG1'length loop
      RESULTS(I) := ROUND(ARG1(I));
    end loop;

--      compute accuracy

    for I in 1 to ARG1'length loop
      ACCURACY(I) := COMPARE(CORRECT_ANSWERS(I), RESULTS(I));
    end loop;

--      print results

    PRINT_RESULTS("ROUND", ARG1, RESULTS, CORRECT_ANSWERS, ACCURACY, FULL_RESULTS);

    wait;

  end process ROUND_TEST;

  TRUNC_TEST : process

    constant ARG1 : REALX_VECTOR := (

      0.0,
      1.0,
      -1.0,
      5.0,
      -5.0,
      4.3,
      4.7,
      4.5,
      -4.3,
      -4.7,
      -4.5,
      -1.0E38,
      1.0E38);


    constant CORRECT_ANSWERS : REALX_VECTOR(1 to ARG1'length) := (

      0.0,
      1.0,
      -1.0,
      5.0,
      -5.0,
      4.0,
      4.0,
      4.0,
      -4.0,
      -4.0,
      -4.0,
      -1.0E38,
      1.0E38);

    variable RESULTS  : REALX_VECTOR(1 to ARG1'length);
    variable ACCURACY : INT_VECTOR(1 to ARG1'length);

  begin

    wait for 5 ns;

--      compute results

    for I in 1 to ARG1'length loop
      RESULTS(I) := TRUNC(ARG1(I));
    end loop;

--      compute accuracy

    for I in 1 to ARG1'length loop
      ACCURACY(I) := COMPARE(CORRECT_ANSWERS(I), RESULTS(I));
    end loop;

--      print results

    PRINT_RESULTS("TRUNC", ARG1, RESULTS, CORRECT_ANSWERS, ACCURACY, FULL_RESULTS);

    wait;

  end process TRUNC_TEST;

  MOD_TEST : process

    constant ARG1 : REALX_VECTOR := (

      0.0,
      5.0,
      5.0,
      -5.0,
      -5.0
      );

    constant ARG2 : REALX_VECTOR(1 to ARG1'length) := (

      1.0,
      3.0,
      -3.0,
      3.0,
      -3.0

      );

    constant CORRECT_ANSWERS : REALX_VECTOR(1 to ARG1'length) := (

      0.0,
      2.0,
      -1.0,
      1.0,
      -2.0


      );

    variable RESULTS  : REALX_VECTOR(1 to ARG1'length);
    variable ACCURACY : INT_VECTOR(1 to ARG1'length);

  begin

    wait for 6 ns;

--      compute results

    for I in 1 to ARG1'length loop
      RESULTS(I) := "MOD"(ARG1(I), ARG2(I));
    end loop;

--      compute accuracy

    for I in 1 to ARG1'length loop
      ACCURACY(I) := COMPARE(CORRECT_ANSWERS(I), RESULTS(I));
    end loop;

--      print results

    PRINT_RESULTS("MOD", ARG1, ARG2, RESULTS, CORRECT_ANSWERS, ACCURACY, FULL_RESULTS);

    wait;

  end process MOD_TEST;

  REALMAX_TEST : process

    constant ARG1 : REALX_VECTOR := (

      0.0,
      5.0,
      5.0,
      -5.0,
      -5.0
      );

    constant ARG2 : REALX_VECTOR(1 to ARG1'length) := (

      1.0,
      3.0,
      -3.0,
      3.0,
      -3.0

      );

    constant CORRECT_ANSWERS : REALX_VECTOR(1 to ARG1'length) := (

      1.0,
      5.0,
      5.0,
      3.0,
      -3.0


      );

    variable RESULTS  : REALX_VECTOR(1 to ARG1'length);
    variable ACCURACY : INT_VECTOR(1 to ARG1'length);

  begin

    wait for 7 ns;

--      compute results

    for I in 1 to ARG1'length loop
      RESULTS(I) := REALMAX(ARG1(I), ARG2(I));
    end loop;

--      compute accuracy

    for I in 1 to ARG1'length loop
      ACCURACY(I) := COMPARE(CORRECT_ANSWERS(I), RESULTS(I));
    end loop;

--      print results

    PRINT_RESULTS("REALMAX", ARG1, ARG2, RESULTS, CORRECT_ANSWERS, ACCURACY, FULL_RESULTS);

    wait;

  end process REALMAX_TEST;

  REALMIN_TEST : process

    constant ARG1 : REALX_VECTOR := (

      0.0,
      5.0,
      5.0,
      -5.0,
      -5.0
      );

    constant ARG2 : REALX_VECTOR(1 to ARG1'length) := (

      1.0,
      3.0,
      -3.0,
      3.0,
      -3.0

      );

    constant CORRECT_ANSWERS : REALX_VECTOR(1 to ARG1'length) := (

      0.0,
      3.0,
      -3.0,
      -5.0,
      -5.0


      );

    variable RESULTS  : REALX_VECTOR(1 to ARG1'length);
    variable ACCURACY : INT_VECTOR(1 to ARG1'length);

  begin

    wait for 8 ns;

--      compute results

    for I in 1 to ARG1'length loop
      RESULTS(I) := REALMIN(ARG1(I), ARG2(I));
    end loop;

--      compute accuracy

    for I in 1 to ARG1'length loop
      ACCURACY(I) := COMPARE(CORRECT_ANSWERS(I), RESULTS(I));
    end loop;

--      print results

    PRINT_RESULTS("REALMIN", ARG1, ARG2, RESULTS, CORRECT_ANSWERS, ACCURACY, FULL_RESULTS);

    wait;

  end process REALMIN_TEST;

  UNIFORM_TEST : process

--      The results of this test were determined empirically
--      The positive seed values produced should agree exactly, even if the
--      real random numbers are not exact.

    constant SEED1 : INT_VECTOR := (

      1,
      1,
      2147483562,
      2147483562,
      100,
      1000

      );

    constant SEED2 : INT_VECTOR(1 to SEED1'length) := (
      1,
      2147483398,
      1,
      2147483398,
      1492,
      1066

      );

    constant EXPECTED_SEED1 : INT_VECTOR(1 to SEED1'length) := (

      40014,
      40014,
      2147443549,
      2147443549,
      4001400,
      40014000

      );

    constant EXPECTED_SEED2 : INT_VECTOR(1 to SEED1'length) := (

      40692,
      2147442707,
      40692,
      2147442707,
      60712464,
      43377672

      );

    constant EXPECTED_RESULTS : REALX_VECTOR(1 to SEED1'length) := (

      9.999996714911893E-1,
      3.76575636697E-5,
      9.999624060143342E-1,
      3.920868146E-7,
      9.735918394229275E-1,
      9.98433655333257E-1

      );

    variable S1, S2 : POSITIVE;
    variable RSLT   : REAL;

    variable RESULTS              : REALX_VECTOR(1 to SEED1'length);
    variable NEW_SEED1, NEW_SEED2 : INT_VECTOR(1 to SEED1'length);


  begin

    wait for 9 ns;

--      compute results

    for I in 1 to SEED1'length loop
      S1           := SEED1(I);
      S2           := SEED2(I);
      UNIFORM(S1, S2, RSLT);
      NEW_SEED1(I) := S1;
      NEW_SEED2(I) := S2;
      RESULTS(I)   := RSLT;

    end loop;

--      print results

    PRINT_RESULTS(SEED1, SEED2, NEW_SEED1, NEW_SEED2, RESULTS,
                  EXPECTED_SEED1, EXPECTED_SEED2, EXPECTED_RESULTS, FULL_RESULTS);

    wait;

  end process UNIFORM_TEST;

  SQRT_TEST : process

    constant ARG1 : REALX_VECTOR := (

      0.0,
      1.00000000000000E+00,
      3.00000000000000E+00,
      2.18700000000000E+03,
      1.59432300000000E+06,
      1.16226146700000E+09,
      8.47288609443000E+11,
      6.17673396283947E+14,
      4.50283905890997E+17,
      3.28256967394537E+20,
      2.39299329230618E+23,
      1.74449211009120E+26,
      1.27173474825649E+29,
      9.27094631478979E+31,
      6.75851986348175E+34,
      4.92696098047820E+37,
      3.33333333333333E-01,
      1.11111111111111E-01,
      1.52415790275873E-04,
      2.09075158128769E-07,
      2.86797199079244E-10,
      3.93411795719128E-13,
      5.39659527735429E-16,
      7.40273700597296E-19,
      1.01546460987283E-21,
      1.39295556909854E-24,
      1.91077581494998E-27,
      2.62109165288064E-30,
      3.59546180093366E-33,
      4.93204636616414E-36);

    constant CORRECT_ANSWERS : REALX_VECTOR(1 to ARG1'length) := (

      0.0,
      1.00000000000000E+00,
      1.73205080756888E+00,
      4.67653718043597E+01,
      1.26266503871771E+03,
      3.40919560453782E+04,
      9.20482813225212E+05,
      2.48530359570807E+07,
      6.71031970841179E+08,
      1.81178632127118E+10,
      4.89182306743220E+11,
      1.32079222820669E+13,
      3.56613901615808E+14,
      9.62857534362680E+15,
      2.59971534277923E+17,
      7.01923142550393E+18,
      5.77350269189625E-01,
      3.33333333333333E-01,
      1.23456790123457E-02,
      4.57247370827618E-04,
      1.69350878084303E-05,
      6.27225474386307E-07,
      2.32305731254188E-08,
      8.60391597237732E-10,
      3.18663554532493E-11,
      1.18023538715738E-12,
      4.37124217465697E-14,
      1.61897858320629E-15,
      5.99621697483810E-17,
      2.22082110179189E-18);

    variable RESULTS  : REALX_VECTOR(1 to ARG1'length);
    variable ACCURACY : INT_VECTOR(1 to ARG1'length);

  begin

    wait for 10 ns;

--      compute results

    for I in 1 to ARG1'length loop
      RESULTS(I) := SQRT(ARG1(I));
    end loop;

--      compute accuracy

    for I in 1 to ARG1'length loop
      ACCURACY(I) := COMPARE(CORRECT_ANSWERS(I), RESULTS(I));
    end loop;

--      print results

    PRINT_RESULTS("SQRT", ARG1, RESULTS, CORRECT_ANSWERS, ACCURACY, FULL_RESULTS);

    wait;

  end process SQRT_TEST;

  CBRT_TEST : process

    constant ARG1 : REALX_VECTOR := (

      0.0,
      1.00000000000000E+00,
      -1.0,
      3.00000000000000E+00,
      2.18700000000000E+03,
      1.59432300000000E+06,
      1.16226146700000E+09,
      8.47288609443000E+11,
      6.17673396283947E+14,
      4.50283905890997E+17,
      3.28256967394537E+20,
      2.39299329230618E+23,
      1.74449211009120E+26,
      1.27173474825649E+29,
      9.27094631478979E+31,
      6.75851986348175E+34,
      4.92696098047820E+37,
      3.33333333333333E-01,
      1.11111111111111E-01,
      1.52415790275873E-04,
      2.09075158128769E-07,
      2.86797199079244E-10,
      3.93411795719128E-13,
      5.39659527735429E-16,
      7.40273700597296E-19,
      1.01546460987283E-21,
      1.39295556909854E-24,
      1.91077581494998E-27,
      2.62109165288064E-30,
      3.59546180093366E-33,
      4.93204636616414E-36,
      -8.0);

    constant CORRECT_ANSWERS : REALX_VECTOR(1 to ARG1'length) := (

      0.0,
      1.00000000000000E+00,
      -1.0,
      1.44224957030741E+00,
      1.29802461327667E+01,
      1.16822215194900E+02,
      1.05139993675410E+03,
      9.46259943078691E+03,
      8.51633948770822E+04,
      7.66470553893739E+05,
      6.89823498504365E+06,
      6.20841148653929E+07,
      5.58757033788536E+08,
      5.02881330409683E+09,
      4.52593197368714E+10,
      4.07333877631843E+11,
      3.66600489868659E+12,
      6.93361274350634E-01,
      4.80749856769136E-01,
      5.34166507521263E-02,
      5.93518341690292E-03,
      6.59464824100324E-04,
      7.32738693444805E-05,
      8.14154103827560E-06,
      9.04615670919511E-07,
      1.00512852324390E-07,
      1.11680947027100E-08,
      1.24089941141222E-09,
      1.37877712379136E-10,
      1.53197458199040E-11,
      1.70219397998933E-12,
      -2.0);

    variable RESULTS  : REALX_VECTOR(1 to ARG1'length);
    variable ACCURACY : INT_VECTOR(1 to ARG1'length);

  begin

    wait for 11 ns;

--      compute results

    for I in 1 to ARG1'length loop
      RESULTS(I) := CBRT(ARG1(I));
    end loop;

--      compute accuracy

    for I in 1 to ARG1'length loop
      ACCURACY(I) := COMPARE(CORRECT_ANSWERS(I), RESULTS(I));
    end loop;

--      print results

    PRINT_RESULTS("CBRT", ARG1, RESULTS, CORRECT_ANSWERS, ACCURACY, FULL_RESULTS);

    wait;

  end process CBRT_TEST;

  POWER_TEST1 : process

    constant ARG1 : INT_VECTOR := (

      1,
      -1,
      0,
      1,
      0,
      -2,
      25,
      256,
      5,
      4,
      4,
      4,
      1
      );

    constant ARG2 : REALX_VECTOR(1 to ARG1'length) := (

      0.0,
      0.0,
      1.0,
      1.0,
      2.0,
      0.0,
      0.5,
      0.25,
      2.0,
      3.0,
      -0.5,
      1.0,
      50.0
      );

    constant CORRECT_ANSWERS : REALX_VECTOR(1 to ARG1'length) := (

      1.0,
      1.0,
      0.0,
      1.0,
      0.0,
      1.0,
      5.0,
      4.0,
      25.0,
      64.0,
      0.5,
      4.0,
      1.0
      );

    variable RESULTS   : REALX_VECTOR(1 to ARG1'length);
    variable ACCURACY  : INT_VECTOR(1 to ARG1'length);
    variable REAL_ARG1 : REALX_VECTOR(1 to ARG1'length);
  begin

    wait for 12 ns;

--      compute results

    for I in 1 to ARG1'length loop
      RESULTS(I) := "**"(ARG1(I), ARG2(I));
    end loop;

--      compute accuracy

    for I in 1 to ARG1'length loop
      ACCURACY(I) := COMPARE(CORRECT_ANSWERS(I), RESULTS(I));
    end loop;

--      translate integer argument 1 to real for more uniform printing

    for I in 1 to ARG1'length loop
      REAL_ARG1(I) := REAL(ARG1(I));
    end loop;

--      print results

    PRINT_RESULTS("**[int,real]", REAL_ARG1, ARG2, RESULTS, CORRECT_ANSWERS, ACCURACY, FULL_RESULTS);

    wait;

  end process POWER_TEST1;

  POWER_TEST2 : process

    constant ARG1 : REALX_VECTOR := (

      1.0,
      -1.0,
      0.0,
      1.0,
      0.0,
      -2.0,
      25.0,
      256.0,
      5.0,
      4.0,
      4.0,
      4.0,
      1.0,
      0.5
      );

    constant ARG2 : REALX_VECTOR(1 to ARG1'length) := (

      0.0,
      0.0,
      1.0,
      1.0,
      2.0,
      0.0,
      0.5,
      0.25,
      2.0,
      3.0,
      -0.5,
      1.0,
      50.0,
      -2.0
      );

    constant CORRECT_ANSWERS : REALX_VECTOR(1 to ARG1'length) := (

      1.0,
      1.0,
      0.0,
      1.0,
      0.0,
      1.0,
      5.0,
      4.0,
      25.0,
      64.0,
      0.5,
      4.0,
      1.0,
      4.0
      );

    variable RESULTS   : REALX_VECTOR(1 to ARG1'length);
    variable ACCURACY  : INT_VECTOR(1 to ARG1'length);
    variable REAL_ARG1 : REALX_VECTOR(1 to ARG1'length);
  begin

    wait for 13 ns;

--      compute results

    for I in 1 to ARG1'length loop
      RESULTS(I) := "**"(ARG1(I), ARG2(I));
    end loop;

--      compute accuracy

    for I in 1 to ARG1'length loop
      ACCURACY(I) := COMPARE(CORRECT_ANSWERS(I), RESULTS(I));
    end loop;

--      print results

    PRINT_RESULTS("**[real,real]", ARG1, ARG2, RESULTS, CORRECT_ANSWERS, ACCURACY, FULL_RESULTS);

    wait;

  end process POWER_TEST2;

  EXP_TEST : process

    constant ARG1 : REALX_VECTOR := (

      -7.50000000000000E+01,
      -5.00000000000000E+01,
      -2.50000000000000E+01,
      0.00000000000000E+00,
      2.50000000000000E+01,
      5.00000000000000E+01,
      7.50000000000000E+01,
      -1.00000000000000E+01,
      -9.00000000000000E+00,
      -8.00000000000000E+00,
      -7.00000000000000E+00,
      -6.00000000000000E+00,
      -5.00000000000000E+00,
      -4.00000000000000E+00,
      -3.00000000000000E+00,
      -2.00000000000000E+00,
      -1.00000000000000E+00,
      0.00000000000000E+00,
      1.00000000000000E+00,
      2.00000000000000E+00,
      3.00000000000000E+00,
      4.00000000000000E+00,
      5.00000000000000E+00,
      6.00000000000000E+00,
      7.00000000000000E+00,
      8.00000000000000E+00,
      9.00000000000000E+00,
      1.00000000000000E+01);

    constant CORRECT_ANSWERS : REALX_VECTOR(1 to ARG1'length) := (

      2.67863696180808E-33,
      1.92874984796392E-22,
      1.38879438649640E-11,
      1.00000000000000E+00,
      7.20048993373859E+10,
      5.18470552858707E+21,
      3.73324199679900E+32,
      4.53999297624849E-05,
      1.23409804086680E-04,
      3.35462627902512E-04,
      9.11881965554516E-04,
      2.47875217666636E-03,
      6.73794699908547E-03,
      1.83156388887342E-02,
      4.97870683678639E-02,
      1.35335283236613E-01,
      MATH_1_OVER_E,
      1.00000000000000E+00,
      MATH_E,
      7.38905609893065E+00,
      2.00855369231877E+01,
      5.45981500331442E+01,
      1.48413159102577E+02,
      4.03428793492735E+02,
      1.09663315842846E+03,
      2.98095798704173E+03,
      8.10308392757538E+03,
      2.20264657948067E+04);

    variable RESULTS  : REALX_VECTOR(1 to ARG1'length);
    variable ACCURACY : INT_VECTOR(1 to ARG1'length);

  begin

    wait for 14 ns;

--      compute results

    for I in 1 to ARG1'length loop
      RESULTS(I) := EXP(ARG1(I));
    end loop;

--      compute accuracy

    for I in 1 to ARG1'length loop
      ACCURACY(I) := COMPARE(CORRECT_ANSWERS(I), RESULTS(I));
    end loop;

--      print results

    PRINT_RESULTS("EXP", ARG1, RESULTS, CORRECT_ANSWERS, ACCURACY, FULL_RESULTS);

    wait;

  end process EXP_TEST;

  LN_TEST : process

    constant ARG1 : REALX_VECTOR := (

      1.00000000000000E+00,
      MATH_E,
      3.00000000000000E+00,
      9.00000000000000E+00,
      2.70000000000000E+01,
      8.10000000000000E+01,
      5.90490000000000E+04,
      4.30467210000000E+07,
      3.13810596090000E+10,
      2.28767924549610E+13,
      1.66771816996666E+16,
      1.21576654590569E+19,
      8.86293811965250E+21,
      6.46108188922667E+24,
      4.71012869724624E+27,
      3.43368382029251E+30,
      2.50315550499324E+33,
      1.82480036314007E+36,
      3.33333333333333E-01,
      1.11111111111111E-01,
      3.70370370370370E-02,
      5.08052634252909E-05,
      6.96917193762563E-08,
      9.55990663597481E-11,
      1.31137265239709E-13,
      1.79886509245143E-16,
      2.46757900199099E-19,
      3.38488203290945E-22,
      4.64318523032846E-25,
      6.36925271649995E-28,
      8.73697217626879E-31,
      1.19848726697789E-33,
      1.64401545538805E-36);

    constant CORRECT_ANSWERS : REALX_VECTOR(1 to ARG1'length) := (

      0.00000000000000E+00,
      1.0,
      1.09861228866811E+00,
      2.19722457733622E+00,
      3.29583686600433E+00,
      4.39444915467244E+00,
      1.09861228866811E+01,
      1.75777966186898E+01,
      2.41694703506984E+01,
      3.07611440827071E+01,
      3.73528178147157E+01,
      4.39444915467244E+01,
      5.05361652787330E+01,
      5.71278390107417E+01,
      6.37195127427504E+01,
      7.03111864747590E+01,
      7.69028602067677E+01,
      8.34945339387763E+01,
      -1.09861228866811E+00,
      -2.19722457733622E+00,
      -3.29583686600433E+00,
      -9.88751059801299E+00,
      -1.64791843300216E+01,
      -2.30708580620303E+01,
      -2.96625317940390E+01,
      -3.62542055260476E+01,
      -4.28458792580563E+01,
      -4.94375529900649E+01,
      -5.60292267220736E+01,
      -6.26209004540823E+01,
      -6.92125741860909E+01,
      -7.58042479180996E+01,
      -8.23959216501082E+01);


    variable RESULTS  : REALX_VECTOR(1 to ARG1'length);
    variable ACCURACY : INT_VECTOR(1 to ARG1'length);

  begin

    wait for 15 ns;

--      compute results

    for I in 1 to ARG1'length loop
      RESULTS(I) := LOG(ARG1(I));
    end loop;

--      compute accuracy

    for I in 1 to ARG1'length loop
      ACCURACY(I) := COMPARE(CORRECT_ANSWERS(I), RESULTS(I));
    end loop;

--      print results

    PRINT_RESULTS("LOG", ARG1, RESULTS, CORRECT_ANSWERS, ACCURACY, FULL_RESULTS);

    wait;

  end process LN_TEST;

  LOG2_TEST : process

    constant ARG1 : REALX_VECTOR := (

      1.0,
      2.0,
      0.5,
      8.0,
      0.125,
      1024.0);


    constant CORRECT_ANSWERS : REALX_VECTOR(1 to ARG1'length) := (

      0.0,
      1.0,
      -1.0,
      3.0,
      -3.0,
      10.0);

    variable RESULTS  : REALX_VECTOR(1 to ARG1'length);
    variable ACCURACY : INT_VECTOR(1 to ARG1'length);

  begin

    wait for 16 ns;

--      compute results

    for I in 1 to ARG1'length loop
      RESULTS(I) := LOG2(ARG1(I));
    end loop;

--      compute accuracy

    for I in 1 to ARG1'length loop
      ACCURACY(I) := COMPARE(CORRECT_ANSWERS(I), RESULTS(I));
    end loop;

--      print results

    PRINT_RESULTS("LOG2", ARG1, RESULTS, CORRECT_ANSWERS, ACCURACY, FULL_RESULTS);

    wait;

  end process LOG2_TEST;

  LOG10_TEST : process

    constant ARG1 : REALX_VECTOR := (

      1.00000000000000E+00,
      10.0,
      3.00000000000000E+00,
      9.00000000000000E+00,
      2.70000000000000E+01,
      8.10000000000000E+01,
      2.43000000000000E+02,
      1.77147000000000E+05,
      1.29140163000000E+08,
      9.41431788270000E+10,
      6.86303773648830E+13,
      5.00315450989997E+16,
      3.64729963771708E+19,
      2.65888143589575E+22,
      1.93832456676800E+25,
      1.41303860917387E+28,
      1.03010514608775E+31,
      7.50946651497973E+33,
      5.47440108942022E+36,
      3.33333333333333E-01,
      1.11111111111111E-01,
      3.70370370370370E-02,
      5.08052634252909E-05,
      6.96917193762563E-08,
      9.55990663597481E-11,
      1.31137265239709E-13,
      1.79886509245143E-16,
      2.46757900199099E-19,
      3.38488203290945E-22,
      4.64318523032846E-25,
      6.36925271649995E-28,
      8.73697217626879E-31,
      1.19848726697789E-33,
      1.64401545538805E-36);

    constant CORRECT_ANSWERS : REALX_VECTOR(1 to ARG1'length) := (

      0.00000000000000E+00,
      1.0,
      4.77121254719663E-01,
      9.54242509439325E-01,
      1.43136376415899E+00,
      1.90848501887865E+00,
      2.38560627359831E+00,
      5.24833380191629E+00,
      8.11106133023426E+00,
      1.09737888585522E+01,
      1.38365163868702E+01,
      1.66992439151882E+01,
      1.95619714435062E+01,
      2.24246989718241E+01,
      2.52874265001421E+01,
      2.81501540284601E+01,
      3.10128815567781E+01,
      3.38756090850960E+01,
      3.67383366134140E+01,
      -4.77121254719663E-01,
      -9.54242509439325E-01,
      -1.43136376415899E+00,
      -4.29409129247696E+00,
      -7.15681882079494E+00,
      -1.00195463491129E+01,
      -1.28822738774309E+01,
      -1.57450014057489E+01,
      -1.86077289340668E+01,
      -2.14704564623848E+01,
      -2.43331839907028E+01,
      -2.71959115190208E+01,
      -3.00586390473387E+01,
      -3.29213665756567E+01,
      -3.57840941039747E+01);

    variable RESULTS  : REALX_VECTOR(1 to ARG1'length);
    variable ACCURACY : INT_VECTOR(1 to ARG1'length);

  begin

    wait for 17 ns;

--      compute results

    for I in 1 to ARG1'length loop
      RESULTS(I) := LOG10(ARG1(I));
    end loop;

--      compute accuracy

    for I in 1 to ARG1'length loop
      ACCURACY(I) := COMPARE(CORRECT_ANSWERS(I), RESULTS(I));
    end loop;

--      print results

    PRINT_RESULTS("LOG10", ARG1, RESULTS, CORRECT_ANSWERS, ACCURACY, FULL_RESULTS);

    wait;

  end process LOG10_TEST;

  LOG_TEST : process

    constant ARG1 : REALX_VECTOR := (

      1.00000000000000E+00,
      3.00000000000000E+00,
      9.00000000000000E+00,
      2.70000000000000E+01,
      8.10000000000000E+01,
      2.43000000000000E+02,
      1.77147000000000E+05,
      1.29140163000000E+08,
      9.41431788270000E+10,
      6.86303773648830E+13,
      5.00315450989997E+16,
      3.64729963771708E+19,
      2.65888143589575E+22,
      1.93832456676800E+25,
      1.41303860917387E+28,
      1.03010514608775E+31,
      7.50946651497973E+33,
      5.47440108942022E+36,
      3.33333333333333E-01,
      1.11111111111111E-01,
      3.70370370370370E-02,
      5.08052634252909E-05,
      6.96917193762563E-08,
      9.55990663597481E-11,
      1.31137265239709E-13,
      1.79886509245143E-16,
      2.46757900199099E-19,
      3.38488203290945E-22,
      4.64318523032846E-25,
      6.36925271649995E-28,
      8.73697217626879E-31,
      1.19848726697789E-33,
      1.64401545538805E-36,
      1.0,
      2.0,
      0.5,
      8.0,
      0.125,
      1024.0,
      6.25

      );

    constant ARG2 : REALX_VECTOR(1 to ARG1'length) := (

      10.0,
      10.0,
      10.0,
      10.0,
      10.0,
      10.0,
      10.0,
      10.0,
      10.0,
      10.0,
      10.0,
      10.0,
      10.0,
      10.0,
      10.0,
      10.0,
      10.0,
      10.0,
      10.0,
      10.0,
      10.0,
      10.0,
      10.0,
      10.0,
      10.0,
      10.0,
      10.0,
      10.0,
      10.0,
      10.0,
      10.0,
      10.0,
      10.0,
      2.0,
      2.0,
      2.0,
      2.0,
      2.0,
      2.0,
      2.5

      );

    constant CORRECT_ANSWERS : REALX_VECTOR(1 to ARG1'length) := (

      0.00000000000000E+00,
      4.77121254719663E-01,
      9.54242509439325E-01,
      1.43136376415899E+00,
      1.90848501887865E+00,
      2.38560627359831E+00,
      5.24833380191629E+00,
      8.11106133023426E+00,
      1.09737888585522E+01,
      1.38365163868702E+01,
      1.66992439151882E+01,
      1.95619714435062E+01,
      2.24246989718241E+01,
      2.52874265001421E+01,
      2.81501540284601E+01,
      3.10128815567781E+01,
      3.38756090850960E+01,
      3.67383366134140E+01,
      -4.77121254719663E-01,
      -9.54242509439325E-01,
      -1.43136376415899E+00,
      -4.29409129247696E+00,
      -7.15681882079494E+00,
      -1.00195463491129E+01,
      -1.28822738774309E+01,
      -1.57450014057489E+01,
      -1.86077289340668E+01,
      -2.14704564623848E+01,
      -2.43331839907028E+01,
      -2.71959115190208E+01,
      -3.00586390473387E+01,
      -3.29213665756567E+01,
      -3.57840941039747E+01,
      0.0,
      1.0,
      -1.0,
      3.0,
      -3.0,
      10.0,
      2.0);

    variable RESULTS  : REALX_VECTOR(1 to ARG1'length);
    variable ACCURACY : INT_VECTOR(1 to ARG1'length);

  begin

    wait for 18 ns;

--      compute results

    for I in 1 to ARG1'length loop
      RESULTS(I) := LOG(ARG1(I), ARG2(I));
    end loop;

--      compute accuracy

    for I in 1 to ARG1'length loop
      ACCURACY(I) := COMPARE(CORRECT_ANSWERS(I), RESULTS(I));
    end loop;

--      print results

    PRINT_RESULTS("LOG(X,BASE)", ARG1, ARG2, RESULTS, CORRECT_ANSWERS, ACCURACY, FULL_RESULTS);

    wait;

  end process LOG_TEST;

  SIN_TEST : process
    constant ARG1 : REALX_VECTOR := (

      0.00000000000000E+00,
      1.0*MATH_PI,
      2.0*MATH_PI,
      -1.0*MATH_PI,
      -2.0*MATH_PI,
      1.0*MATH_PI_OVER_2,
      5.0*MATH_PI_OVER_2,
      9.0*MATH_PI_OVER_2,
      -3.0*MATH_PI_OVER_2,
      -7.0*MATH_PI_OVER_2,
      3.0*MATH_PI_OVER_2,
      7.0*MATH_PI_OVER_2,
      11.0*MATH_PI_OVER_2,
      -1.0*MATH_PI_OVER_2,
      -5.0*MATH_PI_OVER_2,
      5.23598775598299E-01,
      7.85398163397448E-01,
      1.04719755119660E+00,
      1.57079632679490E+00,
      2.09439510239320E+00,
      2.35619449019234E+00,
      2.61799387799149E+00,
      3.1415926535897901E+00,
      3.66519142918809E+00,
      3.92699081698724E+00,
      4.18879020478639E+00,
      4.71238898038469E+00,
      5.23598775598299E+00,
      5.49778714378214E+00,
      5.75958653158129E+00,
      6.28318530717959E+00,
      1.57079632679490E+01,
      8.16814089933346E+01,
      -8.16814089933346E+01,
      8.48230016469244E+01,
      -8.48230016469244E+01,
      8.32522053201295E+01,
      1.00000000000000E+00,
      2.00000000000000E+00);

    constant CORRECT_ANSWERS : REALX_VECTOR(1 to ARG1'length) := (

      0.00000000000000E+00,
      0.00000000000000E+00,
      0.00000000000000E+00,
      0.00000000000000E+00,
      0.00000000000000E+00,
      1.0,
      1.0,
      1.0,
      1.0,
      1.0,
      -1.0,
      -1.0,
      -1.0,
      -1.0,
      -1.0,
      5.00000000000000E-01,
      7.07106781186547E-01,
      8.66025403784440E-01,
      1.00000000000000E+00,
      8.66025403784436E-01,
      7.07106781186551E-01,
      5.00000000000004E-01,
      0.0,
      -4.99999999999998E-01,
      -7.07106781186546E-01,
      -8.66025403784438E-01,
      -1.00000000000000E+00,
      -8.66025403784438E-01,
      -7.07106781186546E-01,
      -4.99999999999998E-01,
      0.0,
      1.419869022E-08,
      0.0,
      0.0,
      1.419869022E-08,
      -1.419869022E-08,
      1.00000000000000E+00,
      8.41470984807897E-01,
      9.09297426825682E-01);

    variable RESULTS  : REALX_VECTOR(1 to ARG1'length);
    variable ACCURACY : INT_VECTOR(1 to ARG1'length);

  begin
    wait for 19 ns;

--      compute results

    for I in 1 to ARG1'length loop
      RESULTS(I) := SIN(ARG1(I));
    end loop;

--      compute accuracy

    for I in 1 to ARG1'length loop
      ACCURACY(I) := COMPARE(CORRECT_ANSWERS(I), RESULTS(I));
    end loop;

--      print results

    PRINT_RESULTS("SIN", ARG1, RESULTS, CORRECT_ANSWERS, ACCURACY, FULL_RESULTS);

    wait;

  end process SIN_TEST;

  COS_TEST : process
    constant ARG1 : REALX_VECTOR := (
      1.0*MATH_PI_OVER_2,
      3.0*MATH_PI_OVER_2,
      5.0*MATH_PI_OVER_2,
      -1.0*MATH_PI_OVER_2,
      -3.0*MATH_PI_OVER_2,
      0.0*MATH_PI,
      2.0*MATH_PI,
      4.0*MATH_PI,
      -2.0*MATH_PI,
      -4.0*MATH_PI,
      1.0*MATH_PI,
      3.0*MATH_PI,
      5.0*MATH_PI,
      -1.0*MATH_PI,
      -3.0*MATH_PI,
      0.00000000000000E+00,
      5.23598775598299E-01,
      7.85398163397448E-01,
      1.04719755119660E+00,
      1.57079632679490E+00,
      2.09439510239320E+00,
      2.35619449019234E+00,
      2.61799387799149E+00,
      3.14159265358979E+00,
      3.66519142918809E+00,
      3.92699081698724E+00,
      4.18879020478639E+00,
      4.71238898038469E+00,
      5.23598775598299E+00,
      5.49778714378214E+00,
      5.75958653158129E+00,
      6.28318530717959E+00,
      1.57079632679490E+01,
      8.16814089933346E+01,
      -8.16814089933346E+01,
      8.48230016469244E+01,
      -8.48230016469244E+01,
      8.325220532012951E+01,
      1.00000000000000E+00,
      2.00000000000000E+00);

    constant CORRECT_ANSWERS : REALX_VECTOR(1 to ARG1'length) := (

      0.0,
      0.0,
      0.0,
      0.0,
      0.0,
      1.0,
      1.0,
      1.0,
      1.0,
      1.0,
      -1.0,
      -1.0,
      -1.0,
      -1.0,
      -1.0,
      1.00000000000000E+00,
      8.66025403784439E-01,
      7.07106781186548E-01,
      4.99999999999998E-01,
      0.0,
      -5.00000000000004E-01,
      -7.07106781186544E-01,
      -8.66025403784436E-01,
      -1.00000000000000E+00,
      -8.66025403784440E-01,
      -7.07106781186549E-01,
      -5.00000000000001E-01,
      0.0,
      5.00000000000001E-01,
      7.07106781186549E-01,
      8.66025403784440E-01,
      1.00000000000000E+00,
      -1.00000000000000E+00,
      1.00000000000000E+00,
      1.00000000000000E+00,
      -1.00000000000000E+00,
      -1.00000000000000E+00,
      7.09934511E-09,
      5.40302305868140E-01,
      -4.16146836547142E-01);

    variable RESULTS  : REALX_VECTOR(1 to ARG1'length);
    variable ACCURACY : INT_VECTOR(1 to ARG1'length);

  begin
    wait for 20 ns;

--      compute results

    for I in 1 to ARG1'length loop
      RESULTS(I) := COS(ARG1(I));
    end loop;

--      compute accuracy

    for I in 1 to ARG1'length loop
      ACCURACY(I) := COMPARE(CORRECT_ANSWERS(I), RESULTS(I));
    end loop;

--      print results

    PRINT_RESULTS("COS", ARG1, RESULTS, CORRECT_ANSWERS, ACCURACY, FULL_RESULTS);

    wait;

  end process COS_TEST;

  TAN_TEST : process

    constant ARG1 : REALX_VECTOR := (

      0.00000000000000E+00,
      MATH_PI,
      2.0*MATH_PI,
      -MATH_PI,
      -2.0*MATH_PI,
      5.23598775598299E-01,
      7.85398163397448E-01,
      1.04719755119660E+00,
      2.09439510239320E+00,
      2.35619449019234E+00,
      2.61799387799149E+00,
      3.1415926535897901E+00,
      3.66519142918809E+00,
      3.92699081698724E+00,
      4.18879020478639E+00,
      5.23598775598299E+00,
      5.49778714378214E+00,
      5.75958653158129E+00,
      6.28318530717959E+00,
      1.57079632679490E+01,
      8.16814089933346E+01,
      -8.16814089933346E+01,
      8.48230016469244E+01,
      -8.48230016469244E+01,
      1.00000000000000E+00,
      2.00000000000000E+00);

    constant CORRECT_ANSWERS : REALX_VECTOR(1 to ARG1'length) := (

      0.00000000000000E+00,
      0.00000000000000E+00,
      0.00000000000000E+00,
      0.00000000000000E+00,
      0.00000000000000E+00,
      5.77350269189626E-01,
      9.99999999999999E-01,
      1.73205080756889E+00,
      -1.73205080756886E+00,
      -1.00000000000001E+00,
      -5.77350269189632E-01,
      0.0,
      5.77350269189623E-01,
      9.99999999999997E-01,
      1.73205080756887E+00,
      -1.73205080756887E+00,
      -9.99999999999997E-01,
      -5.77350269189623E-01,
      0.0,
      -1.023944006E-08,
      0.0,
      0.0,
      -1.419869022E-08,
      1.419869022E-08,
      1.55740772465490E+00,
      -2.18503986326152E+00);

    variable RESULTS  : REALX_VECTOR(1 to ARG1'length);
    variable ACCURACY : INT_VECTOR(1 to ARG1'length);

  begin

    wait for 21 ns;

--      compute results

    for I in 1 to ARG1'length loop
      RESULTS(I) := TAN(ARG1(I));
    end loop;

--      compute accuracy

    for I in 1 to ARG1'length loop
      ACCURACY(I) := COMPARE(CORRECT_ANSWERS(I), RESULTS(I));
    end loop;

--      print results

    PRINT_RESULTS("TAN", ARG1, RESULTS, CORRECT_ANSWERS, ACCURACY, FULL_RESULTS);

    wait;

  end process TAN_TEST;

  ARCSIN_TEST : process

    constant ARG1 : REALX_VECTOR := (

      -1.00000000000000E+00,
      0.00000000000000E+00,
      1.00000000000000E+00,
      -1.00000000000000E+00,
      -9.00000000000000E-01,
      -8.00000000000000E-01,
      -7.00000000000000E-01,
      -6.00000000000000E-01,
      -5.00000000000000E-01,
      -4.00000000000000E-01,
      -3.00000000000000E-01,
      -2.00000000000000E-01,
      -1.00000000000000E-01,
      -1.38777878078145E-16,
      9.99999999999999E-02,
      2.00000000000000E-01,
      3.00000000000000E-01,
      4.00000000000000E-01,
      5.00000000000000E-01,
      6.00000000000000E-01,
      7.00000000000000E-01,
      8.00000000000000E-01,
      9.00000000000000E-01,
      1.00000000000000E+00);

    constant CORRECT_ANSWERS : REALX_VECTOR(1 to ARG1'length) := (

      -MATH_PI_OVER_2,
      0.00000000000000E+00,
      MATH_PI_OVER_2,
      -1.57079632679490E+00,
      -1.11976951499863E+00,
      -9.27295218001612E-01,
      -7.75397496610753E-01,
      -6.43501108793284E-01,
      -5.23598775598299E-01,
      -4.11516846067488E-01,
      -3.04692654015398E-01,
      -2.01357920790331E-01,
      -1.00167421161560E-01,
      -1.38777878078145E-16,
      1.00167421161560E-01,
      2.01357920790331E-01,
      3.04692654015398E-01,
      4.11516846067488E-01,
      5.23598775598299E-01,
      6.43501108793284E-01,
      7.75397496610753E-01,
      9.27295218001612E-01,
      1.11976951499863E+00,
      1.57079632679490E+00);

    variable RESULTS  : REALX_VECTOR(1 to ARG1'length);
    variable ACCURACY : INT_VECTOR(1 to ARG1'length);

  begin

    wait for 22 ns;

--      compute results

    for I in 1 to ARG1'length loop
      RESULTS(I) := ARCSIN(ARG1(I));
    end loop;

--      compute accuracy

    for I in 1 to ARG1'length loop
      ACCURACY(I) := COMPARE(CORRECT_ANSWERS(I), RESULTS(I));
    end loop;

--      print results

    PRINT_RESULTS("ARCSIN", ARG1, RESULTS, CORRECT_ANSWERS, ACCURACY, FULL_RESULTS);

    wait;

  end process ARCSIN_TEST;

  ARCCOS_TEST : process

    constant ARG1 : REALX_VECTOR := (

      -1.00000000000000E+00,
      0.00000000000000E+00,
      1.00000000000000E+00,
      -1.00000000000000E+00,
      -9.00000000000000E-01,
      -8.00000000000000E-01,
      -7.00000000000000E-01,
      -6.00000000000000E-01,
      -5.00000000000000E-01,
      -4.00000000000000E-01,
      -3.00000000000000E-01,
      -2.00000000000000E-01,
      -1.00000000000000E-01,
      -1.38777878078145E-16,
      9.99999999999999E-02,
      2.00000000000000E-01,
      3.00000000000000E-01,
      4.00000000000000E-01,
      5.00000000000000E-01,
      6.00000000000000E-01,
      7.00000000000000E-01,
      8.00000000000000E-01,
      9.00000000000000E-01,
      1.00000000000000E+00);


    constant CORRECT_ANSWERS : REALX_VECTOR(1 to ARG1'length) := (

      MATH_PI,
      MATH_PI_OVER_2,
      0.00000000000000E+00,
      3.14159265358979E+00,
      2.69056584179353E+00,
      2.49809154479651E+00,
      2.34619382340565E+00,
      2.21429743558818E+00,
      2.09439510239320E+00,
      1.98231317286238E+00,
      1.87548898081029E+00,
      1.77215424758523E+00,
      1.67096374795646E+00,
      1.57079632679490E+00,
      1.47062890563334E+00,
      1.36943840600457E+00,
      1.26610367277950E+00,
      1.15927948072741E+00,
      1.04719755119660E+00,
      9.27295218001612E-01,
      7.95398830184144E-01,
      6.43501108793284E-01,
      4.51026811796262E-01,
      0.00000000000000E+00);

    variable RESULTS  : REALX_VECTOR(1 to ARG1'length);
    variable ACCURACY : INT_VECTOR(1 to ARG1'length);

  begin

    wait for 23 ns;

--      compute results

    for I in 1 to ARG1'length loop
      RESULTS(I) := ARCCOS(ARG1(I));
    end loop;

--      compute accuracy

    for I in 1 to ARG1'length loop
      ACCURACY(I) := COMPARE(CORRECT_ANSWERS(I), RESULTS(I));
    end loop;

--      print results

    PRINT_RESULTS("ARCCOS", ARG1, RESULTS, CORRECT_ANSWERS, ACCURACY, FULL_RESULTS);

    wait;

  end process ARCCOS_TEST;

  ARCTAN_TEST : process

    constant ARG1 : REALX_VECTOR := (

      -1.00500000000000E+03,
      -9.05000000000000E+02,
      -8.05000000000000E+02,
      -7.05000000000000E+02,
      -6.05000000000000E+02,
      -5.05000000000000E+02,
      -4.05000000000000E+02,
      -3.05000000000000E+02,
      -2.05000000000000E+02,
      -1.05000000000000E+02,
      -5.00000000000000E+00,
      -4.50000000000000E+00,
      -4.00000000000000E+00,
      -3.50000000000000E+00,
      -3.00000000000000E+00,
      -2.50000000000000E+00,
      -2.00000000000000E+00,
      -1.50000000000000E+00,
      -1.00000000000000E+00,
      -5.00000000000000E-01,
      0.00000000000000E+00,
      5.00000000000000E-01,
      1.00000000000000E+00,
      1.50000000000000E+00,
      2.00000000000000E+00,
      2.50000000000000E+00,
      3.00000000000000E+00,
      3.50000000000000E+00,
      4.00000000000000E+00,
      4.50000000000000E+00,
      5.00000000000000E+00,
      1.05000000000000E+02,
      2.05000000000000E+02,
      3.05000000000000E+02,
      4.05000000000000E+02,
      5.05000000000000E+02,
      6.05000000000000E+02,
      7.05000000000000E+02,
      8.05000000000000E+02,
      9.05000000000000E+02);

    constant CORRECT_ANSWERS : REALX_VECTOR(1 to ARG1'length) := (

      -1.56980130224766E+00,
      -1.56969135486892E+00,
      -1.56955409140904E+00,
      -1.56937788802987E+00,
      -1.56914343573817E+00,
      -1.56881613136333E+00,
      -1.56832719601021E+00,
      -1.56751765001864E+00,
      -1.56591831670550E+00,
      -1.56127280520128E+00,
      -1.37340076694502E+00,
      -1.35212738092095E+00,
      -1.32581766366803E+00,
      -1.29249666778979E+00,
      -1.24904577239825E+00,
      -1.19028994968253E+00,
      -1.10714871779409E+00,
      -9.82793723247329E-01,
      -7.85398163397448E-01,
      -4.63647609000806E-01,
      0.00000000000000E+00,
      4.63647609000806E-01,
      7.85398163397448E-01,
      9.82793723247329E-01,
      1.10714871779409E+00,
      1.19028994968253E+00,
      1.24904577239825E+00,
      1.29249666778979E+00,
      1.32581766366803E+00,
      1.35212738092095E+00,
      1.37340076694502E+00,
      1.56127280520128E+00,
      1.56591831670550E+00,
      1.56751765001864E+00,
      1.56832719601021E+00,
      1.56881613136333E+00,
      1.56914343573817E+00,
      1.56937788802987E+00,
      1.56955409140904E+00,
      1.56969135486892E+00);


    variable RESULTS  : REALX_VECTOR(1 to ARG1'length);
    variable ACCURACY : INT_VECTOR(1 to ARG1'length);

  begin

    wait for 24 ns;

--      compute results

    for I in 1 to ARG1'length loop
      RESULTS(I) := ARCTAN(ARG1(I));
    end loop;

--      compute accuracy

    for I in 1 to ARG1'length loop
      ACCURACY(I) := COMPARE(CORRECT_ANSWERS(I), RESULTS(I));
    end loop;

--      print results

    PRINT_RESULTS("ARCTAN", ARG1, RESULTS, CORRECT_ANSWERS, ACCURACY, FULL_RESULTS);

    wait;

  end process ARCTAN_TEST;

  ARCTAN2_TEST : process

    constant ARG1 : REALX_VECTOR := (

      0.0,
      0.0,
      3.0,
      -3.0,

      -1.00100000000000E+03,
      -1.00100000000000E+03,
      -1.00100000000000E+03,
      -1.00100000000000E+03,
      -1.00100000000000E+03,
      -1.00100000000000E+03,
      -1.00100000000000E+03,
      -1.00100000000000E+03,
      -1.00100000000000E+03,
      -1.00100000000000E+03,
      -1.00100000000000E+03,
      -1.00100000000000E+03,
      -1.00100000000000E+03,
      -1.00100000000000E+03,
      -1.00100000000000E+03,
      -1.00100000000000E+03,
      -1.00100000000000E+03,
      -1.00100000000000E+03,
      -1.00100000000000E+03,
      -1.00100000000000E+03,
      -1.00100000000000E+03,

      -9.01000000000000E+02,
      -9.01000000000000E+02,
      -9.01000000000000E+02,
      -9.01000000000000E+02,
      -9.01000000000000E+02,
      -9.01000000000000E+02,
      -9.01000000000000E+02,
      -9.01000000000000E+02,
      -9.01000000000000E+02,
      -9.01000000000000E+02,
      -9.01000000000000E+02,
      -9.01000000000000E+02,
      -9.01000000000000E+02,
      -9.01000000000000E+02,
      -9.01000000000000E+02,
      -9.01000000000000E+02,
      -9.01000000000000E+02,
      -9.01000000000000E+02,
      -9.01000000000000E+02,
      -9.01000000000000E+02,
      -9.01000000000000E+02,

      -8.01000000000000E+02,
      -8.01000000000000E+02,
      -8.01000000000000E+02,
      -8.01000000000000E+02,
      -8.01000000000000E+02,
      -8.01000000000000E+02,
      -8.01000000000000E+02,
      -8.01000000000000E+02,
      -8.01000000000000E+02,
      -8.01000000000000E+02,
      -8.01000000000000E+02,
      -8.01000000000000E+02,
      -8.01000000000000E+02,
      -8.01000000000000E+02,
      -8.01000000000000E+02,
      -8.01000000000000E+02,
      -8.01000000000000E+02,
      -8.01000000000000E+02,
      -8.01000000000000E+02,
      -8.01000000000000E+02,
      -8.01000000000000E+02,

      -7.01000000000000E+02,
      -7.01000000000000E+02,
      -7.01000000000000E+02,
      -7.01000000000000E+02,
      -7.01000000000000E+02,
      -7.01000000000000E+02,
      -7.01000000000000E+02,
      -7.01000000000000E+02,
      -7.01000000000000E+02,
      -7.01000000000000E+02,
      -7.01000000000000E+02,
      -7.01000000000000E+02,
      -7.01000000000000E+02,
      -7.01000000000000E+02,
      -7.01000000000000E+02,
      -7.01000000000000E+02,
      -7.01000000000000E+02,
      -7.01000000000000E+02,
      -7.01000000000000E+02,
      -7.01000000000000E+02,
      -7.01000000000000E+02,

      -6.01000000000000E+02,
      -6.01000000000000E+02,
      -6.01000000000000E+02,
      -6.01000000000000E+02,
      -6.01000000000000E+02,
      -6.01000000000000E+02,
      -6.01000000000000E+02,
      -6.01000000000000E+02,
      -6.01000000000000E+02,
      -6.01000000000000E+02,
      -6.01000000000000E+02,
      -6.01000000000000E+02,
      -6.01000000000000E+02,
      -6.01000000000000E+02,
      -6.01000000000000E+02,
      -6.01000000000000E+02,
      -6.01000000000000E+02,
      -6.01000000000000E+02,
      -6.01000000000000E+02,
      -6.01000000000000E+02,
      -6.01000000000000E+02,

      -5.01000000000000E+02,
      -5.01000000000000E+02,
      -5.01000000000000E+02,
      -5.01000000000000E+02,
      -5.01000000000000E+02,
      -5.01000000000000E+02,
      -5.01000000000000E+02,
      -5.01000000000000E+02,
      -5.01000000000000E+02,
      -5.01000000000000E+02,
      -5.01000000000000E+02,
      -5.01000000000000E+02,
      -5.01000000000000E+02,
      -5.01000000000000E+02,
      -5.01000000000000E+02,
      -5.01000000000000E+02,
      -5.01000000000000E+02,
      -5.01000000000000E+02,
      -5.01000000000000E+02,
      -5.01000000000000E+02,
      -5.01000000000000E+02,

      -4.01000000000000E+02,
      -4.01000000000000E+02,
      -4.01000000000000E+02,
      -4.01000000000000E+02,
      -4.01000000000000E+02,
      -4.01000000000000E+02,
      -4.01000000000000E+02,
      -4.01000000000000E+02,
      -4.01000000000000E+02,
      -4.01000000000000E+02,
      -4.01000000000000E+02,
      -4.01000000000000E+02,
      -4.01000000000000E+02,
      -4.01000000000000E+02,
      -4.01000000000000E+02,
      -4.01000000000000E+02,
      -4.01000000000000E+02,
      -4.01000000000000E+02,
      -4.01000000000000E+02,
      -4.01000000000000E+02,
      -4.01000000000000E+02,

      -3.01000000000000E+02,
      -3.01000000000000E+02,
      -3.01000000000000E+02,
      -3.01000000000000E+02,
      -3.01000000000000E+02,
      -3.01000000000000E+02,
      -3.01000000000000E+02,
      -3.01000000000000E+02,
      -3.01000000000000E+02,
      -3.01000000000000E+02,
      -3.01000000000000E+02,
      -3.01000000000000E+02,
      -3.01000000000000E+02,
      -3.01000000000000E+02,
      -3.01000000000000E+02,
      -3.01000000000000E+02,
      -3.01000000000000E+02,
      -3.01000000000000E+02,
      -3.01000000000000E+02,
      -3.01000000000000E+02,
      -3.01000000000000E+02,

      -2.01000000000000E+02,
      -2.01000000000000E+02,
      -2.01000000000000E+02,
      -2.01000000000000E+02,
      -2.01000000000000E+02,
      -2.01000000000000E+02,
      -2.01000000000000E+02,
      -2.01000000000000E+02,
      -2.01000000000000E+02,
      -2.01000000000000E+02,
      -2.01000000000000E+02,
      -2.01000000000000E+02,
      -2.01000000000000E+02,
      -2.01000000000000E+02,
      -2.01000000000000E+02,
      -2.01000000000000E+02,
      -2.01000000000000E+02,
      -2.01000000000000E+02,
      -2.01000000000000E+02,
      -2.01000000000000E+02,
      -2.01000000000000E+02,

      -1.01000000000000E+02,
      -1.01000000000000E+02,
      -1.01000000000000E+02,
      -1.01000000000000E+02,
      -1.01000000000000E+02,
      -1.01000000000000E+02,
      -1.01000000000000E+02,
      -1.01000000000000E+02,
      -1.01000000000000E+02,
      -1.01000000000000E+02,
      -1.01000000000000E+02,
      -1.01000000000000E+02,
      -1.01000000000000E+02,
      -1.01000000000000E+02,
      -1.01000000000000E+02,
      -1.01000000000000E+02,
      -1.01000000000000E+02,
      -1.01000000000000E+02,
      -1.01000000000000E+02,
      -1.01000000000000E+02,
      -1.01000000000000E+02,

      -1.00000000000000E+00,
      -1.00000000000000E+00,
      -1.00000000000000E+00,
      -1.00000000000000E+00,
      -1.00000000000000E+00,
      -1.00000000000000E+00,
      -1.00000000000000E+00,
      -1.00000000000000E+00,
      -1.00000000000000E+00,
      -1.00000000000000E+00,
      -1.00000000000000E+00,
      -1.00000000000000E+00,
      -1.00000000000000E+00,
      -1.00000000000000E+00,
      -1.00000000000000E+00,
      -1.00000000000000E+00,
      -1.00000000000000E+00,
      -1.00000000000000E+00,
      -1.00000000000000E+00,
      -1.00000000000000E+00,
      -1.00000000000000E+00,

      9.90000000000000E+01,
      9.90000000000000E+01,
      9.90000000000000E+01,
      9.90000000000000E+01,
      9.90000000000000E+01,
      9.90000000000000E+01,
      9.90000000000000E+01,
      9.90000000000000E+01,
      9.90000000000000E+01,
      9.90000000000000E+01,
      9.90000000000000E+01,
      9.90000000000000E+01,
      9.90000000000000E+01,
      9.90000000000000E+01,
      9.90000000000000E+01,
      9.90000000000000E+01,
      9.90000000000000E+01,
      9.90000000000000E+01,
      9.90000000000000E+01,
      9.90000000000000E+01,
      9.90000000000000E+01,

      1.99000000000000E+02,
      1.99000000000000E+02,
      1.99000000000000E+02,
      1.99000000000000E+02,
      1.99000000000000E+02,
      1.99000000000000E+02,
      1.99000000000000E+02,
      1.99000000000000E+02,
      1.99000000000000E+02,
      1.99000000000000E+02,
      1.99000000000000E+02,
      1.99000000000000E+02,
      1.99000000000000E+02,
      1.99000000000000E+02,
      1.99000000000000E+02,
      1.99000000000000E+02,
      1.99000000000000E+02,
      1.99000000000000E+02,
      1.99000000000000E+02,
      1.99000000000000E+02,
      1.99000000000000E+02,

      2.99000000000000E+02,
      2.99000000000000E+02,
      2.99000000000000E+02,
      2.99000000000000E+02,
      2.99000000000000E+02,
      2.99000000000000E+02,
      2.99000000000000E+02,
      2.99000000000000E+02,
      2.99000000000000E+02,
      2.99000000000000E+02,
      2.99000000000000E+02,
      2.99000000000000E+02,
      2.99000000000000E+02,
      2.99000000000000E+02,
      2.99000000000000E+02,
      2.99000000000000E+02,
      2.99000000000000E+02,
      2.99000000000000E+02,
      2.99000000000000E+02,
      2.99000000000000E+02,
      2.99000000000000E+02,

      3.99000000000000E+02,
      3.99000000000000E+02,
      3.99000000000000E+02,
      3.99000000000000E+02,
      3.99000000000000E+02,
      3.99000000000000E+02,
      3.99000000000000E+02,
      3.99000000000000E+02,
      3.99000000000000E+02,
      3.99000000000000E+02,
      3.99000000000000E+02,
      3.99000000000000E+02,
      3.99000000000000E+02,
      3.99000000000000E+02,
      3.99000000000000E+02,
      3.99000000000000E+02,
      3.99000000000000E+02,
      3.99000000000000E+02,
      3.99000000000000E+02,
      3.99000000000000E+02,
      3.99000000000000E+02,

      4.99000000000000E+02,
      4.99000000000000E+02,
      4.99000000000000E+02,
      4.99000000000000E+02,
      4.99000000000000E+02,
      4.99000000000000E+02,
      4.99000000000000E+02,
      4.99000000000000E+02,
      4.99000000000000E+02,
      4.99000000000000E+02,
      4.99000000000000E+02,
      4.99000000000000E+02,
      4.99000000000000E+02,
      4.99000000000000E+02,
      4.99000000000000E+02,
      4.99000000000000E+02,
      4.99000000000000E+02,
      4.99000000000000E+02,
      4.99000000000000E+02,
      4.99000000000000E+02,
      4.99000000000000E+02,

      5.99000000000000E+02,
      5.99000000000000E+02,
      5.99000000000000E+02,
      5.99000000000000E+02,
      5.99000000000000E+02,
      5.99000000000000E+02,
      5.99000000000000E+02,
      5.99000000000000E+02,
      5.99000000000000E+02,
      5.99000000000000E+02,
      5.99000000000000E+02,
      5.99000000000000E+02,
      5.99000000000000E+02,
      5.99000000000000E+02,
      5.99000000000000E+02,
      5.99000000000000E+02,
      5.99000000000000E+02,
      5.99000000000000E+02,
      5.99000000000000E+02,
      5.99000000000000E+02,
      5.99000000000000E+02,

      6.99000000000000E+02,
      6.99000000000000E+02,
      6.99000000000000E+02,
      6.99000000000000E+02,
      6.99000000000000E+02,
      6.99000000000000E+02,
      6.99000000000000E+02,
      6.99000000000000E+02,
      6.99000000000000E+02,
      6.99000000000000E+02,
      6.99000000000000E+02,
      6.99000000000000E+02,
      6.99000000000000E+02,
      6.99000000000000E+02,
      6.99000000000000E+02,
      6.99000000000000E+02,
      6.99000000000000E+02,
      6.99000000000000E+02,
      6.99000000000000E+02,
      6.99000000000000E+02,
      6.99000000000000E+02,

      7.99000000000000E+02,
      7.99000000000000E+02,
      7.99000000000000E+02,
      7.99000000000000E+02,
      7.99000000000000E+02,
      7.99000000000000E+02,
      7.99000000000000E+02,
      7.99000000000000E+02,
      7.99000000000000E+02,
      7.99000000000000E+02,
      7.99000000000000E+02,
      7.99000000000000E+02,
      7.99000000000000E+02,
      7.99000000000000E+02,
      7.99000000000000E+02,
      7.99000000000000E+02,
      7.99000000000000E+02,
      7.99000000000000E+02,
      7.99000000000000E+02,
      7.99000000000000E+02,
      7.99000000000000E+02,

      8.99000000000000E+02,
      8.99000000000000E+02,
      8.99000000000000E+02,
      8.99000000000000E+02,
      8.99000000000000E+02,
      8.99000000000000E+02,
      8.99000000000000E+02,
      8.99000000000000E+02,
      8.99000000000000E+02,
      8.99000000000000E+02,
      8.99000000000000E+02,
      8.99000000000000E+02,
      8.99000000000000E+02,
      8.99000000000000E+02,
      8.99000000000000E+02,
      8.99000000000000E+02,
      8.99000000000000E+02,
      8.99000000000000E+02,
      8.99000000000000E+02,
      8.99000000000000E+02,
      8.99000000000000E+02,

      9.99000000000000E+02,
      9.99000000000000E+02,
      9.99000000000000E+02,
      9.99000000000000E+02,
      9.99000000000000E+02,
      9.99000000000000E+02,
      9.99000000000000E+02,
      9.99000000000000E+02,
      9.99000000000000E+02,
      9.99000000000000E+02,
      9.99000000000000E+02,
      9.99000000000000E+02,
      9.99000000000000E+02,
      9.99000000000000E+02,
      9.99000000000000E+02,
      9.99000000000000E+02,
      9.99000000000000E+02,
      9.99000000000000E+02,
      9.99000000000000E+02,
      9.99000000000000E+02,
      9.99000000000000E+02);

    constant ARG2 : REALX_VECTOR(1 to ARG1'length) := (

      3.0,
      -3.0,
      0.0,
      0.0,

      -1.00100000000000E+03,
      -9.01000000000000E+02,
      -8.01000000000000E+02,
      -7.01000000000000E+02,
      -6.01000000000000E+02,
      -5.01000000000000E+02,
      -4.01000000000000E+02,
      -3.01000000000000E+02,
      -2.01000000000000E+02,
      -1.01000000000000E+02,
      -1.00000000000000E+00,
      9.90000000000000E+01,
      1.99000000000000E+02,
      2.99000000000000E+02,
      3.99000000000000E+02,
      4.99000000000000E+02,
      5.99000000000000E+02,
      6.99000000000000E+02,
      7.99000000000000E+02,
      8.99000000000000E+02,
      9.99000000000000E+02,

      -1.00100000000000E+03,
      -9.01000000000000E+02,
      -8.01000000000000E+02,
      -7.01000000000000E+02,
      -6.01000000000000E+02,
      -5.01000000000000E+02,
      -4.01000000000000E+02,
      -3.01000000000000E+02,
      -2.01000000000000E+02,
      -1.01000000000000E+02,
      -1.00000000000000E+00,
      9.90000000000000E+01,
      1.99000000000000E+02,
      2.99000000000000E+02,
      3.99000000000000E+02,
      4.99000000000000E+02,
      5.99000000000000E+02,
      6.99000000000000E+02,
      7.99000000000000E+02,
      8.99000000000000E+02,
      9.99000000000000E+02,

      -1.00100000000000E+03,
      -9.01000000000000E+02,
      -8.01000000000000E+02,
      -7.01000000000000E+02,
      -6.01000000000000E+02,
      -5.01000000000000E+02,
      -4.01000000000000E+02,
      -3.01000000000000E+02,
      -2.01000000000000E+02,
      -1.01000000000000E+02,
      -1.00000000000000E+00,
      9.90000000000000E+01,
      1.99000000000000E+02,
      2.99000000000000E+02,
      3.99000000000000E+02,
      4.99000000000000E+02,
      5.99000000000000E+02,
      6.99000000000000E+02,
      7.99000000000000E+02,
      8.99000000000000E+02,
      9.99000000000000E+02,

      -1.00100000000000E+03,
      -9.01000000000000E+02,
      -8.01000000000000E+02,
      -7.01000000000000E+02,
      -6.01000000000000E+02,
      -5.01000000000000E+02,
      -4.01000000000000E+02,
      -3.01000000000000E+02,
      -2.01000000000000E+02,
      -1.01000000000000E+02,
      -1.00000000000000E+00,
      9.90000000000000E+01,
      1.99000000000000E+02,
      2.99000000000000E+02,
      3.99000000000000E+02,
      4.99000000000000E+02,
      5.99000000000000E+02,
      6.99000000000000E+02,
      7.99000000000000E+02,
      8.99000000000000E+02,
      9.99000000000000E+02,

      -1.00100000000000E+03,
      -9.01000000000000E+02,
      -8.01000000000000E+02,
      -7.01000000000000E+02,
      -6.01000000000000E+02,
      -5.01000000000000E+02,
      -4.01000000000000E+02,
      -3.01000000000000E+02,
      -2.01000000000000E+02,
      -1.01000000000000E+02,
      -1.00000000000000E+00,
      9.90000000000000E+01,
      1.99000000000000E+02,
      2.99000000000000E+02,
      3.99000000000000E+02,
      4.99000000000000E+02,
      5.99000000000000E+02,
      6.99000000000000E+02,
      7.99000000000000E+02,
      8.99000000000000E+02,
      9.99000000000000E+02,

      -1.00100000000000E+03,
      -9.01000000000000E+02,
      -8.01000000000000E+02,
      -7.01000000000000E+02,
      -6.01000000000000E+02,
      -5.01000000000000E+02,
      -4.01000000000000E+02,
      -3.01000000000000E+02,
      -2.01000000000000E+02,
      -1.01000000000000E+02,
      -1.00000000000000E+00,
      9.90000000000000E+01,
      1.99000000000000E+02,
      2.99000000000000E+02,
      3.99000000000000E+02,
      4.99000000000000E+02,
      5.99000000000000E+02,
      6.99000000000000E+02,
      7.99000000000000E+02,
      8.99000000000000E+02,
      9.99000000000000E+02,

      -1.00100000000000E+03,
      -9.01000000000000E+02,
      -8.01000000000000E+02,
      -7.01000000000000E+02,
      -6.01000000000000E+02,
      -5.01000000000000E+02,
      -4.01000000000000E+02,
      -3.01000000000000E+02,
      -2.01000000000000E+02,
      -1.01000000000000E+02,
      -1.00000000000000E+00,
      9.90000000000000E+01,
      1.99000000000000E+02,
      2.99000000000000E+02,
      3.99000000000000E+02,
      4.99000000000000E+02,
      5.99000000000000E+02,
      6.99000000000000E+02,
      7.99000000000000E+02,
      8.99000000000000E+02,
      9.99000000000000E+02,

      -1.00100000000000E+03,
      -9.01000000000000E+02,
      -8.01000000000000E+02,
      -7.01000000000000E+02,
      -6.01000000000000E+02,
      -5.01000000000000E+02,
      -4.01000000000000E+02,
      -3.01000000000000E+02,
      -2.01000000000000E+02,
      -1.01000000000000E+02,
      -1.00000000000000E+00,
      9.90000000000000E+01,
      1.99000000000000E+02,
      2.99000000000000E+02,
      3.99000000000000E+02,
      4.99000000000000E+02,
      5.99000000000000E+02,
      6.99000000000000E+02,
      7.99000000000000E+02,
      8.99000000000000E+02,
      9.99000000000000E+02,

      -1.00100000000000E+03,
      -9.01000000000000E+02,
      -8.01000000000000E+02,
      -7.01000000000000E+02,
      -6.01000000000000E+02,
      -5.01000000000000E+02,
      -4.01000000000000E+02,
      -3.01000000000000E+02,
      -2.01000000000000E+02,
      -1.01000000000000E+02,
      -1.00000000000000E+00,
      9.90000000000000E+01,
      1.99000000000000E+02,
      2.99000000000000E+02,
      3.99000000000000E+02,
      4.99000000000000E+02,
      5.99000000000000E+02,
      6.99000000000000E+02,
      7.99000000000000E+02,
      8.99000000000000E+02,
      9.99000000000000E+02,

      -1.00100000000000E+03,
      -9.01000000000000E+02,
      -8.01000000000000E+02,
      -7.01000000000000E+02,
      -6.01000000000000E+02,
      -5.01000000000000E+02,
      -4.01000000000000E+02,
      -3.01000000000000E+02,
      -2.01000000000000E+02,
      -1.01000000000000E+02,
      -1.00000000000000E+00,
      9.90000000000000E+01,
      1.99000000000000E+02,
      2.99000000000000E+02,
      3.99000000000000E+02,
      4.99000000000000E+02,
      5.99000000000000E+02,
      6.99000000000000E+02,
      7.99000000000000E+02,
      8.99000000000000E+02,
      9.99000000000000E+02,

      -1.00100000000000E+03,
      -9.01000000000000E+02,
      -8.01000000000000E+02,
      -7.01000000000000E+02,
      -6.01000000000000E+02,
      -5.01000000000000E+02,
      -4.01000000000000E+02,
      -3.01000000000000E+02,
      -2.01000000000000E+02,
      -1.01000000000000E+02,
      -1.00000000000000E+00,
      9.90000000000000E+01,
      1.99000000000000E+02,
      2.99000000000000E+02,
      3.99000000000000E+02,
      4.99000000000000E+02,
      5.99000000000000E+02,
      6.99000000000000E+02,
      7.99000000000000E+02,
      8.99000000000000E+02,
      9.99000000000000E+02,

      -1.00100000000000E+03,
      -9.01000000000000E+02,
      -8.01000000000000E+02,
      -7.01000000000000E+02,
      -6.01000000000000E+02,
      -5.01000000000000E+02,
      -4.01000000000000E+02,
      -3.01000000000000E+02,
      -2.01000000000000E+02,
      -1.01000000000000E+02,
      -1.00000000000000E+00,
      9.90000000000000E+01,
      1.99000000000000E+02,
      2.99000000000000E+02,
      3.99000000000000E+02,
      4.99000000000000E+02,
      5.99000000000000E+02,
      6.99000000000000E+02,
      7.99000000000000E+02,
      8.99000000000000E+02,
      9.99000000000000E+02,

      -1.00100000000000E+03,
      -9.01000000000000E+02,
      -8.01000000000000E+02,
      -7.01000000000000E+02,
      -6.01000000000000E+02,
      -5.01000000000000E+02,
      -4.01000000000000E+02,
      -3.01000000000000E+02,
      -2.01000000000000E+02,
      -1.01000000000000E+02,
      -1.00000000000000E+00,
      9.90000000000000E+01,
      1.99000000000000E+02,
      2.99000000000000E+02,
      3.99000000000000E+02,
      4.99000000000000E+02,
      5.99000000000000E+02,
      6.99000000000000E+02,
      7.99000000000000E+02,
      8.99000000000000E+02,
      9.99000000000000E+02,

      -1.00100000000000E+03,
      -9.01000000000000E+02,
      -8.01000000000000E+02,
      -7.01000000000000E+02,
      -6.01000000000000E+02,
      -5.01000000000000E+02,
      -4.01000000000000E+02,
      -3.01000000000000E+02,
      -2.01000000000000E+02,
      -1.01000000000000E+02,
      -1.00000000000000E+00,
      9.90000000000000E+01,
      1.99000000000000E+02,
      2.99000000000000E+02,
      3.99000000000000E+02,
      4.99000000000000E+02,
      5.99000000000000E+02,
      6.99000000000000E+02,
      7.99000000000000E+02,
      8.99000000000000E+02,
      9.99000000000000E+02,

      -1.00100000000000E+03,
      -9.01000000000000E+02,
      -8.01000000000000E+02,
      -7.01000000000000E+02,
      -6.01000000000000E+02,
      -5.01000000000000E+02,
      -4.01000000000000E+02,
      -3.01000000000000E+02,
      -2.01000000000000E+02,
      -1.01000000000000E+02,
      -1.00000000000000E+00,
      9.90000000000000E+01,
      1.99000000000000E+02,
      2.99000000000000E+02,
      3.99000000000000E+02,
      4.99000000000000E+02,
      5.99000000000000E+02,
      6.99000000000000E+02,
      7.99000000000000E+02,
      8.99000000000000E+02,
      9.99000000000000E+02,

      -1.00100000000000E+03,
      -9.01000000000000E+02,
      -8.01000000000000E+02,
      -7.01000000000000E+02,
      -6.01000000000000E+02,
      -5.01000000000000E+02,
      -4.01000000000000E+02,
      -3.01000000000000E+02,
      -2.01000000000000E+02,
      -1.01000000000000E+02,
      -1.00000000000000E+00,
      9.90000000000000E+01,
      1.99000000000000E+02,
      2.99000000000000E+02,
      3.99000000000000E+02,
      4.99000000000000E+02,
      5.99000000000000E+02,
      6.99000000000000E+02,
      7.99000000000000E+02,
      8.99000000000000E+02,
      9.99000000000000E+02,

      -1.00100000000000E+03,
      -9.01000000000000E+02,
      -8.01000000000000E+02,
      -7.01000000000000E+02,
      -6.01000000000000E+02,
      -5.01000000000000E+02,
      -4.01000000000000E+02,
      -3.01000000000000E+02,
      -2.01000000000000E+02,
      -1.01000000000000E+02,
      -1.00000000000000E+00,
      9.90000000000000E+01,
      1.99000000000000E+02,
      2.99000000000000E+02,
      3.99000000000000E+02,
      4.99000000000000E+02,
      5.99000000000000E+02,
      6.99000000000000E+02,
      7.99000000000000E+02,
      8.99000000000000E+02,
      9.99000000000000E+02,

      -1.00100000000000E+03,
      -9.01000000000000E+02,
      -8.01000000000000E+02,
      -7.01000000000000E+02,
      -6.01000000000000E+02,
      -5.01000000000000E+02,
      -4.01000000000000E+02,
      -3.01000000000000E+02,
      -2.01000000000000E+02,
      -1.01000000000000E+02,
      -1.00000000000000E+00,
      9.90000000000000E+01,
      1.99000000000000E+02,
      2.99000000000000E+02,
      3.99000000000000E+02,
      4.99000000000000E+02,
      5.99000000000000E+02,
      6.99000000000000E+02,
      7.99000000000000E+02,
      8.99000000000000E+02,
      9.99000000000000E+02,

      -1.00100000000000E+03,
      -9.01000000000000E+02,
      -8.01000000000000E+02,
      -7.01000000000000E+02,
      -6.01000000000000E+02,
      -5.01000000000000E+02,
      -4.01000000000000E+02,
      -3.01000000000000E+02,
      -2.01000000000000E+02,
      -1.01000000000000E+02,
      -1.00000000000000E+00,
      9.90000000000000E+01,
      1.99000000000000E+02,
      2.99000000000000E+02,
      3.99000000000000E+02,
      4.99000000000000E+02,
      5.99000000000000E+02,
      6.99000000000000E+02,
      7.99000000000000E+02,
      8.99000000000000E+02,
      9.99000000000000E+02,

      -1.00100000000000E+03,
      -9.01000000000000E+02,
      -8.01000000000000E+02,
      -7.01000000000000E+02,
      -6.01000000000000E+02,
      -5.01000000000000E+02,
      -4.01000000000000E+02,
      -3.01000000000000E+02,
      -2.01000000000000E+02,
      -1.01000000000000E+02,
      -1.00000000000000E+00,
      9.90000000000000E+01,
      1.99000000000000E+02,
      2.99000000000000E+02,
      3.99000000000000E+02,
      4.99000000000000E+02,
      5.99000000000000E+02,
      6.99000000000000E+02,
      7.99000000000000E+02,
      8.99000000000000E+02,
      9.99000000000000E+02,

      -1.00100000000000E+03,
      -9.01000000000000E+02,
      -8.01000000000000E+02,
      -7.01000000000000E+02,
      -6.01000000000000E+02,
      -5.01000000000000E+02,
      -4.01000000000000E+02,
      -3.01000000000000E+02,
      -2.01000000000000E+02,
      -1.01000000000000E+02,
      -1.00000000000000E+00,
      9.90000000000000E+01,
      1.99000000000000E+02,
      2.99000000000000E+02,
      3.99000000000000E+02,
      4.99000000000000E+02,
      5.99000000000000E+02,
      6.99000000000000E+02,
      7.99000000000000E+02,
      8.99000000000000E+02,
      9.99000000000000E+02);

    constant CORRECT_ANSWERS : REALX_VECTOR(1 to ARG1'length) := (

      0.0,
      MATH_PI,
      MATH_PI_OVER_2,
      -MATH_PI_OVER_2,

      -2.35619449019234E+00,
      -2.30366661926516E+00,
      -2.24565908653521E+00,
      -2.18172340400569E+00,
      -2.11150959908993E+00,
      -2.03484345634976E+00,
      -1.95181932173628E+00,
      -1.86289455800327E+00,
      -1.76896023071179E+00,
      -1.67135509872262E+00,
      -1.57179532746156E+00,
      -1.47221581315609E+00,
      -1.37455372569624E+00,
      -1.28053142686585E+00,
      -1.19149622171506E+00,
      -1.10834823741070E+00,
      -1.03155295065123E+00,
      -9.61211071835094E-01,
      -8.97152811273815E-01,
      -8.39030890387206E-01,
      -7.86398163064115E-01,

      -2.40872236111953E+00,
      -2.35619449019234E+00,
      -2.29750755223232E+00,
      -2.23199315268187E+00,
      -2.15905501228309E+00,
      -2.07827169191935E+00,
      -1.98953543034286E+00,
      -1.89321266005455E+00,
      -1.79028773752862E+00,
      -1.68242796909766E+00,
      -1.57190620425260E+00,
      -1.46135742860496E+00,
      -1.35342043298036E+00,
      -1.25037821664635E+00,
      -1.15391151212805E+00,
      -1.06501807814244E+00,
      -9.84075445180675E-01,
      -9.10983737233827E-01,
      -8.45326318518656E-01,
      -7.86509274051312E-01,
      -7.33864883156771E-01,

      -2.46672989384948E+00,
      -2.41488142815237E+00,
      -2.35619449019234E+00,
      -2.28971470467524E+00,
      -2.21449715597698E+00,
      -2.12973222913349E+00,
      -2.03494318687754E+00,
      -1.93025089792998E+00,
      -1.81665604696251E+00,
      -1.69622675494150E+00,
      -1.57204476559698E+00,
      -1.44782445723060E+00,
      -1.32728695441207E+00,
      -1.21353147331911E+00,
      -1.10864796704559E+00,
      -1.01365719237471E+00,
      -9.28694937143484E-01,
      -8.53293642560509E-01,
      -7.86648162746407E-01,
      -7.27814834799237E-01,
      -6.75838636623504E-01,

      -2.53066557637900E+00,
      -2.48039582770282E+00,
      -2.42267427570945E+00,
      -2.35619449019234E+00,
      -2.27954006632482E+00,
      -2.19131564547489E+00,
      -2.09040319972483E+00,
      -1.97637658112955E+00,
      -1.85003778247013E+00,
      -1.71389146389291E+00,
      -1.57222285935077E+00,
      -1.43049735313154E+00,
      -1.29419317888602E+00,
      -1.16762748849479E+00,
      -1.05334173792262E+00,
      -9.52168022855722E-01,
      -8.63699285330242E-01,
      -7.86826733854204E-01,
      -7.20157549952455E-01,
      -6.62274126837964E-01,
      -6.11867133256799E-01,

      -2.60087938129476E+00,
      -2.55333396810160E+00,
      -2.49789182440771E+00,
      -2.43284891405987E+00,
      -2.35619449019234E+00,
      -2.26569824232904E+00,
      -2.15918280748292E+00,
      -2.03510927169220E+00,
      -1.89354488485221E+00,
      -1.73729380295997E+00,
      -1.57246021877019E+00,
      -1.40753698586366E+00,
      -1.25104377173758E+00,
      -1.10914738268772E+00,
      -9.84716058441887E-01,
      -8.77861031762038E-01,
      -7.87064828520908E-01,
      -7.10155862651441E-01,
      -6.44901387934081E-01,
      -5.89284982942072E-01,
      -5.41596316438125E-01,

      -2.67754552403493E+00,
      -2.63411728846534E+00,
      -2.58265675125120E+00,
      -2.52107333490980E+00,
      -2.44669073805565E+00,
      -2.35619449019234E+00,
      -2.24578063722976E+00,
      -2.11180268145820E+00,
      -1.95233469528497E+00,
      -1.76972680485610E+00,
      -1.57279233212820E+00,
      -1.37570491033282E+00,
      -1.19270124366572E+00,
      -1.03272838009687E+00,
      -8.98249967734331E-01,
      -7.87398160730788E-01,
      -6.96541848597555E-01,
      -6.21871544576455E-01,
      -5.60060480989096E-01,
      -5.08419756926629E-01,
      -4.64848088616192E-01,

      -2.76056965864841E+00,
      -2.72285355004183E+00,
      -2.67744579350715E+00,
      -2.62198578065986E+00,
      -2.55320617290177E+00,
      -2.46660834315493E+00,
      -2.35619449019234E+00,
      -2.21469631869427E+00,
      -2.03544094443843E+00,
      -1.81753451889775E+00,
      -1.57329008721149E+00,
      -1.32875365053432E+00,
      -1.11014571181809E+00,
      -9.30094091140908E-01,
      -7.87898158189135E-01,
      -6.76936596172640E-01,
      -5.89926418026814E-01,
      -5.20839201746805E-01,
      -4.65148358249306E-01,
      -4.19565226145361E-01,
      -3.81713897657087E-01,

      -2.84949442238142E+00,
      -2.81917632033014E+00,
      -2.78213808245471E+00,
      -2.73601239925514E+00,
      -2.67727970869248E+00,
      -2.60058629892649E+00,
      -2.49769266169042E+00,
      -2.35619449019234E+00,
      -2.15956521371738E+00,
      -1.89453891042913E+00,
      -1.57411857370815E+00,
      -1.25303776716068E+00,
      -9.86636901866293E-01,
      -7.88731484385185E-01,
      -6.46302221915377E-01,
      -5.42773821994867E-01,
      -4.65648940551621E-01,
      -4.06617112385411E-01,
      -3.60278551238781E-01,
      -3.23084776420147E-01,
      -2.92650220882730E-01,

      -2.94342874967290E+00,
      -2.92210124285607E+00,
      -2.89573293342218E+00,
      -2.86235119791456E+00,
      -2.81884409553248E+00,
      -2.76005428509972E+00,
      -2.67694803594626E+00,
      -2.55282376666731E+00,
      -2.35619449019234E+00,
      -2.03643200474705E+00,
      -1.57577141012575E+00,
      -1.11313667017800E+00,
      -7.90398121731407E-01,
      -5.91851699242266E-01,
      -4.66650602976804E-01,
      -3.82922665126500E-01,
      -3.23752553723968E-01,
      -2.79999374082147E-01,
      -2.46450549023023E-01,
      -2.19964129419116E-01,
      -1.98550293747721E-01,

      -3.04103388166207E+00,
      -3.02996101128703E+00,
      -3.01616222544319E+00,
      -2.99849751649178E+00,
      -2.97509517742472E+00,
      -2.94266217552859E+00,
      -2.89485446148694E+00,
      -2.81785006995556E+00,
      -2.67595697563764E+00,
      -2.35619449019234E+00,
      -1.58069699338289E+00,
      -7.95397830084114E-01,
      -4.69659560616739E-01,
      -3.25758548967066E-01,
      -2.47925030558522E-01,
      -1.99706803808393E-01,
      -1.67043127110379E-01,
      -1.43498975540674E-01,
      -1.25741101775488E-01,
      -1.11877923692566E-01,
      -1.00758732328938E-01,

      -3.14059365292313E+00,
      -3.14048277613209E+00,
      -3.14034421478771E+00,
      -3.14016612103392E+00,
      -3.13992876161449E+00,
      -3.13959664825649E+00,
      -3.13909889317320E+00,
      -3.13827040667654E+00,
      -3.13661757025894E+00,
      -3.13169198700180E+00,
      -2.35619449019234E+00,
      -1.01006665853219E-02,
      -5.02508333081241E-03,
      -3.34446913547142E-03,
      -2.50626041658822E-03,
      -2.00400533330765E-03,
      -1.66944753085388E-03,
      -1.43061418852778E-03,
      -1.25156380208089E-03,
      -1.11234659350573E-03,
      -1.00100066666587E-03,

      3.04301213995099E+00,
      3.03215375539985E+00,
      3.01862078402550E+00,
      3.00129367992644E+00,
      2.97833331265855E+00,
      2.94650123712772E+00,
      2.89954997732922E+00,
      2.82383409395557E+00,
      2.68393299697290E+00,
      2.36619415687901E+00,
      1.58089699338022E+00,
      7.85398163397448E-01,
      4.61635539281440E-01,
      3.19742524967046E-01,
      2.43208753470570E-01,
      1.95853541025837E-01,
      1.63794765433888E-01,
      1.40695132106014E-01,
      1.23276578696141E-01,
      1.09680420507763E-01,
      9.87765920676627E-02,

      2.94535005249114E+00,
      2.92421675977526E+00,
      2.89808328120697E+00,
      2.86498950568092E+00,
      2.82184009853248E+00,
      2.76349757046062E+00,
      2.68094203861298E+00,
      2.55743322866119E+00,
      2.36119444852630E+00,
      2.04045588741164E+00,
      1.57582141012571E+00,
      1.10916078751346E+00,
      7.85398163397448E-01,
      5.87230402928853E-01,
      4.62644600310076E-01,
      3.79469391656208E-01,
      3.20748550723967E-01,
      2.77354658341414E-01,
      2.44095010926817E-01,
      2.17844349524145E-01,
      1.96625440633627E-01,

      2.85132775366075E+00,
      2.82117454344125E+00,
      2.78432780011400E+00,
      2.73842381528969E+00,
      2.67994370948262E+00,
      2.60352470689177E+00,
      2.50089041793580E+00,
      2.35952781118008E+00,
      2.16264802603716E+00,
      1.89655487576196E+00,
      1.57414079593037E+00,
      1.25105380182785E+00,
      9.83565923866044E-01,
      7.85398163397448E-01,
      6.43099985669993E-01,
      5.39829877697403E-01,
      4.62979606428156E-01,
      4.04200940108289E-01,
      3.58084705225195E-01,
      3.21082997753481E-01,
      2.90813825888230E-01,

      2.76229254850996E+00,
      2.72470783892295E+00,
      2.67944429384049E+00,
      2.62413806471752E+00,
      2.55551238523678E+00,
      2.46904629452923E+00,
      2.35869448498403E+00,
      2.21709854871027E+00,
      2.03744692977170E+00,
      1.81872135735342E+00,
      1.57330258721148E+00,
      1.32758757332433E+00,
      1.10815172648482E+00,
      9.27696341124904E-01,
      7.85398163397448E-01,
      6.74496503215955E-01,
      5.87617247111922E-01,
      5.18683793428791E-01,
      4.63146857915971E-01,
      4.17708173952516E-01,
      3.79988510768188E-01,

      2.67914456420560E+00,
      2.63581440493733E+00,
      2.58445351916960E+00,
      2.52296434965062E+00,
      2.44865735855693E+00,
      2.35819448752568E+00,
      2.24773292296754E+00,
      2.11357014878976E+00,
      1.95371899192140E+00,
      1.77050313060329E+00,
      1.57280033212820E+00,
      1.37494278576906E+00,
      1.19132693513869E+00,
      1.03096644909749E+00,
      8.96299823578942E-01,
      7.85398163397448E-01,
      6.94574045618446E-01,
      6.19978776731178E-01,
      5.58261743622392E-01,
      5.06720646862616E-01,
      4.63247128445524E-01,

      2.60234927744613E+00,
      2.55487177197557E+00,
      2.49949126393838E+00,
      2.43449561212514E+00,
      2.35786115531580E+00,
      2.26733817539245E+00,
      2.16072274482171E+00,
      2.03644526734652E+00,
      1.89454888051886E+00,
      1.73783945390528E+00,
      1.57246577432575E+00,
      1.40700156136101E+00,
      1.25004777607093E+00,
      1.10781672036674E+00,
      9.83179079682975E-01,
      8.76222281176451E-01,
      7.85398163397448E-01,
      7.08508444862986E-01,
      6.43300828403413E-01,
      5.87745864143709E-01,
      5.40125036203712E-01,

      2.53200739862999E+00,
      2.48178006402872E+00,
      2.42408996935541E+00,
      2.35762306064910E+00,
      2.28095218944634E+00,
      2.19266787137135E+00,
      2.09163552854170E+00,
      1.97741343918031E+00,
      1.85079570087704E+00,
      1.71429530233557E+00,
      1.57222694098342E+00,
      1.43010119468888E+00,
      1.29344166845348E+00,
      1.16659538668661E+00,
      1.05211253336611E+00,
      9.50817550063719E-01,
      8.62287881931910E-01,
      7.85398163397448E-01,
      7.18741386418489E-01,
      6.60889133115614E-01,
      6.10524392128282E-01,

      2.46794913806871E+00,
      2.41612264531355E+00,
      2.35744448954130E+00,
      2.29095387674735E+00,
      2.21569771472898E+00,
      2.13085680778399E+00,
      2.03594468504420E+00,
      1.93107487803368E+00,
      1.81724687581792E+00,
      1.69653742857038E+00,
      1.57204789059698E+00,
      1.44751974809876E+00,
      1.32670131586808E+00,
      1.21271162156970E+00,
      1.10764946887893E+00,
      1.01253458317250E+00,
      9.27495498391484E-01,
      8.52054940376408E-01,
      7.85398163397448E-01,
      7.26573294213562E-01,
      6.74618857008679E-01,

      2.40982721718210E+00,
      2.35730560084621E+00,
      2.29861116159413E+00,
      2.23307045363286E+00,
      2.16008130973697E+00,
      2.07921608372153E+00,
      1.99036155294026E+00,
      1.89388110321504E+00,
      1.79076045621401E+00,
      1.68267425048746E+00,
      1.57190867338840E+00,
      1.46111590628713E+00,
      1.35295197727075E+00,
      1.24971332904142E+00,
      1.15308815284238E+00,
      1.06407567993228E+00,
      9.83050462651187E-01,
      9.09907193679282E-01,
      8.44223032581334E-01,
      7.85398163397448E-01,
      7.32759795111047E-01,

      2.35719448985901E+00,
      2.30466120995167E+00,
      2.24663496341840E+00,
      2.18266346005170E+00,
      2.11239264323302E+00,
      2.03564441541109E+00,
      1.95251022445198E+00,
      1.86344654767763E+00,
      1.76934662054262E+00,
      1.67155505912383E+00,
      1.57179732746156E+00,
      1.47201973472723E+00,
      1.37417088616127E+00,
      1.27998250090667E+00,
      1.19080781602671E+00,
      1.10754919834937E+00,
      1.03067129059118E+00,
      9.60271934666615E-01,
      8.96177469786218E-01,
      8.38036531683849E-01,
      7.85398163397448E-01);

    variable RESULTS  : REALX_VECTOR(1 to ARG1'length);
    variable ACCURACY : INT_VECTOR(1 to ARG1'length);

  begin

    wait for 25 ns;

--      compute results

    for I in 1 to ARG1'length loop
      RESULTS(I) := ARCTAN(ARG1(I), ARG2(I));
    end loop;

--      compute accuracy

    for I in 1 to ARG1'length loop
      ACCURACY(I) := COMPARE(CORRECT_ANSWERS(I), RESULTS(I));
    end loop;

--      print results

    PRINT_RESULTS("ARCTAN(Y,X)", ARG1, ARG2, RESULTS, CORRECT_ANSWERS, ACCURACY, FULL_RESULTS);

    wait;

  end process ARCTAN2_TEST;

  SINH_TEST : process

    constant ARG1 : REALX_VECTOR := (

      -6.28318530717959E+01,
      -3.14159265358979E+01,
      -2.51327412287183E+01,
      -1.25663706143593E+01,
      -1.00000000000000E+01,
      -9.50000000000000E+00,
      -9.00000000000000E+00,
      -8.50000000000000E+00,
      -8.00000000000000E+00,
      -7.50000000000000E+00,
      -7.00000000000000E+00,
      -6.50000000000000E+00,
      -6.28318530717959E+00,
      -6.00000000000000E+00,
      -5.50000000000000E+00,
      -5.00000000000000E+00,
      -4.50000000000000E+00,
      -4.00000000000000E+00,
      -3.50000000000000E+00,
      -3.14159265358979E+00,
      -3.00000000000000E+00,
      -2.50000000000000E+00,
      -2.00000000000000E+00,
      -1.57079632679489E+00,
      -1.50000000000000E+00,
      -1.00000000000000E+00,
      -7.85398163397448E-01,
      -5.00000000000000E-01,
      -3.92699081698724E-01,
      -1.96349540849362E-01,
      0.00000000000000E+00,
      1.96349540849362E-01,
      3.92699081698724E-01,
      5.00000000000000E-01,
      7.85398163397448E-01,
      1.00000000000000E+00,
      1.50000000000000E+00,
      1.57079632679489E+00,
      2.00000000000000E+00,
      2.50000000000000E+00,
      3.00000000000000E+00,
      3.14159265358979E+00,
      3.50000000000000E+00,
      4.00000000000000E+00,
      4.50000000000000E+00,
      5.00000000000000E+00,
      5.50000000000000E+00,
      6.00000000000000E+00,
      6.28318530717959E+00,
      6.50000000000000E+00,
      7.00000000000000E+00,
      7.50000000000000E+00,
      8.00000000000000E+00,
      8.50000000000000E+00,
      9.00000000000000E+00,
      9.50000000000000E+00,
      1.00000000000000E+01,
      1.25663706143593E+01,
      2.51327412287183E+01,
      3.14159265358979E+01,
      6.28318530717959E+01);

    constant CORRECT_ANSWERS : REALX_VECTOR(1 to ARG1'length) := (

      -9.69386754177469E+26,
      -2.20157529303153E+13,
      -4.11131577927957E+10,
      -1.43375656566601E+05,
      -1.10132328747034E+04,
      -6.67986337740502E+03,
      -4.05154190208279E+03,
      -2.45738431841538E+03,
      -1.49047882578955E+03,
      -9.04020930685847E+02,
      -5.48316123273247E+02,
      -3.32570064802584E+02,
      -2.67744894041017E+02,
      -2.01713157370279E+02,
      -1.22343922746391E+02,
      -7.42032105777888E+01,
      -4.50030111519918E+01,
      -2.72899171971278E+01,
      -1.65426272876350E+01,
      -1.15487393572577E+01,
      -1.00178749274099E+01,
      -6.05020448103979E+00,
      -3.62686040784702E+00,
      -2.30129890230728E+00,
      -2.12927945509482E+00,
      -1.17520119364380E+00,
      -8.68670961486009E-01,
      -5.21095305493747E-01,
      -4.02870381917066E-01,
      -1.97613623736882E-01,
      0.00000000000000E+00,
      1.97613623736882E-01,
      4.02870381917066E-01,
      5.21095305493747E-01,
      8.68670961486009E-01,
      1.17520119364380E+00,
      2.12927945509482E+00,
      2.30129890230728E+00,
      3.62686040784702E+00,
      6.05020448103979E+00,
      1.00178749274099E+01,
      1.15487393572577E+01,
      1.65426272876350E+01,
      2.72899171971278E+01,
      4.50030111519918E+01,
      7.42032105777888E+01,
      1.22343922746391E+02,
      2.01713157370279E+02,
      2.67744894041017E+02,
      3.32570064802584E+02,
      5.48316123273247E+02,
      9.04020930685847E+02,
      1.49047882578955E+03,
      2.45738431841538E+03,
      4.05154190208279E+03,
      6.67986337740502E+03,
      1.10132328747034E+04,
      1.43375656566601E+05,
      4.11131577927957E+10,
      2.20157529303153E+13,
      9.69386754177469E+26);

    variable RESULTS  : REALX_VECTOR(1 to ARG1'length);
    variable ACCURACY : INT_VECTOR(1 to ARG1'length);

  begin

    wait for 26 ns;

--      compute results

    for I in 1 to ARG1'length loop
      RESULTS(I) := SINH(ARG1(I));
    end loop;

--      compute accuracy

    for I in 1 to ARG1'length loop
      ACCURACY(I) := COMPARE(CORRECT_ANSWERS(I), RESULTS(I));
    end loop;

--      print results

    PRINT_RESULTS("SINH", ARG1, RESULTS, CORRECT_ANSWERS, ACCURACY, FULL_RESULTS);

    wait;

  end process SINH_TEST;

  COSH_TEST : process

    constant ARG1 : REALX_VECTOR := (
      -6.28318530717959E+01,
      -3.14159265358979E+01,
      -2.51327412287183E+01,
      -1.25663706143593E+01,
      -1.00000000000000E+01,
      -9.50000000000000E+00,
      -9.00000000000000E+00,
      -8.50000000000000E+00,
      -8.00000000000000E+00,
      -7.50000000000000E+00,
      -7.00000000000000E+00,
      -6.50000000000000E+00,
      -6.28318530717959E+00,
      -6.00000000000000E+00,
      -5.50000000000000E+00,
      -5.00000000000000E+00,
      -4.50000000000000E+00,
      -4.00000000000000E+00,
      -3.50000000000000E+00,
      -3.14159265358979E+00,
      -3.00000000000000E+00,
      -2.50000000000000E+00,
      -2.00000000000000E+00,
      -1.57079632679489E+00,
      -1.50000000000000E+00,
      -1.00000000000000E+00,
      -7.85398163397448E-01,
      -5.00000000000000E-01,
      -3.92699081698724E-01,
      -1.96349540849362E-01,
      0.00000000000000E+00,
      1.96349540849362E-01,
      3.92699081698724E-01,
      5.00000000000000E-01,
      7.85398163397448E-01,
      1.00000000000000E+00,
      1.50000000000000E+00,
      1.57079632679489E+00,
      2.00000000000000E+00,
      2.50000000000000E+00,
      3.00000000000000E+00,
      3.14159265358979E+00,
      3.50000000000000E+00,
      4.00000000000000E+00,
      4.50000000000000E+00,
      5.00000000000000E+00,
      5.50000000000000E+00,
      6.00000000000000E+00,
      6.28318530717959E+00,
      6.50000000000000E+00,
      7.00000000000000E+00,
      7.50000000000000E+00,
      8.00000000000000E+00,
      8.50000000000000E+00,
      9.00000000000000E+00,
      9.50000000000000E+00,
      1.00000000000000E+01,
      1.25663706143593E+01,
      2.51327412287183E+01,
      3.14159265358979E+01,
      6.28318530717959E+01);

    constant CORRECT_ANSWERS : REALX_VECTOR(1 to ARG1'length) := (

      9.69386754177469E+26,
      2.20157529303153E+13,
      4.11131577927957E+10,
      1.43375656570088E+05,
      1.10132329201033E+04,
      6.67986345225685E+03,
      4.05154202549260E+03,
      2.45738452188375E+03,
      1.49047916125218E+03,
      9.04021483770217E+02,
      5.48317035155212E+02,
      3.32571568241777E+02,
      2.67746761483749E+02,
      2.01715636122456E+02,
      1.22348009517829E+02,
      7.42099485247879E+01,
      4.50141201485300E+01,
      2.73082328360165E+01,
      1.65728246710573E+01,
      1.15919532755215E+01,
      1.00676619957778E+01,
      6.13228947966369E+00,
      3.76219569108363E+00,
      2.50917847865804E+00,
      2.35240961524325E+00,
      1.54308063481524E+00,
      1.32460908925201E+00,
      1.12762596520638E+00,
      1.07810228857284E+00,
      1.01933858177076E+00,
      1.00000000000000E+00,
      1.01933858177076E+00,
      1.07810228857284E+00,
      1.12762596520638E+00,
      1.32460908925201E+00,
      1.54308063481524E+00,
      2.35240961524325E+00,
      2.50917847865804E+00,
      3.76219569108363E+00,
      6.13228947966369E+00,
      1.00676619957778E+01,
      1.15919532755215E+01,
      1.65728246710573E+01,
      2.73082328360165E+01,
      4.50141201485300E+01,
      7.42099485247879E+01,
      1.22348009517829E+02,
      2.01715636122456E+02,
      2.67746761483749E+02,
      3.32571568241777E+02,
      5.48317035155212E+02,
      9.04021483770217E+02,
      1.49047916125218E+03,
      2.45738452188375E+03,
      4.05154202549260E+03,
      6.67986345225685E+03,
      1.10132329201033E+04,
      1.43375656570088E+05,
      4.11131577927957E+10,
      2.20157529303153E+13,
      9.69386754177469E+26);

    variable RESULTS  : REALX_VECTOR(1 to ARG1'length);
    variable ACCURACY : INT_VECTOR(1 to ARG1'length);

  begin

    wait for 27 ns;

--      compute results

    for I in 1 to ARG1'length loop
      RESULTS(I) := COSH(ARG1(I));
    end loop;

--      compute accuracy

    for I in 1 to ARG1'length loop
      ACCURACY(I) := COMPARE(CORRECT_ANSWERS(I), RESULTS(I));
    end loop;

--      print results

    PRINT_RESULTS("COSH", ARG1, RESULTS, CORRECT_ANSWERS, ACCURACY, FULL_RESULTS);

    wait;

  end process COSH_TEST;

  TANH_TEST : process

    constant ARG1 : REALX_VECTOR := (
      -6.28318530717959E+01,
      -3.14159265358979E+01,
      -2.51327412287183E+01,
      -2.00000000000000E+01,
      -1.50000000000000E+01,
      -1.25663706143593E+01,
      -1.00000000000000E+01,
      -6.28318530717959E+00,
      -5.00000000000000E+00,
      -3.14159265358979E+00,
      -2.00000000000000E+00,
      -1.90000000000000E+00,
      -1.80000000000000E+00,
      -1.70000000000000E+00,
      -1.60000000000000E+00,
      -1.57079632679489E+00,
      -1.50000000000000E+00,
      -1.40000000000000E+00,
      -1.30000000000000E+00,
      -1.20000000000000E+00,
      -1.00000000000000E+00,
      -9.00000000000000E-01,
      -8.00000000000000E-01,
      -7.85398163397448E-01,
      -7.00000000000000E-01,
      -6.00000000000000E-01,
      -5.00000000000000E-01,
      -4.00000000000000E-01,
      -3.92699081698724E-01,
      -3.00000000000000E-01,
      -2.00000000000000E-01,
      -1.96349540849362E-01,
      -1.00000000000000E-01,
      0.00000000000000E+00,
      1.00000000000000E-01,
      1.96349540849362E-01,
      2.00000000000000E-01,
      3.00000000000000E-01,
      3.92699081698724E-01,
      4.00000000000000E-01,
      5.00000000000000E-01,
      6.00000000000000E-01,
      7.00000000000000E-01,
      7.85398163397448E-01,
      8.00000000000000E-01,
      9.00000000000000E-01,
      1.00000000000000E+00,
      1.10000000000000E+00,
      1.20000000000000E+00,
      1.30000000000000E+00,
      1.40000000000000E+00,
      1.50000000000000E+00,
      1.57079632679489E+00,
      1.60000000000000E+00,
      1.70000000000000E+00,
      1.80000000000000E+00,
      1.90000000000000E+00,
      2.00000000000000E+00,
      3.14159265358979E+00,
      5.00000000000000E+00,
      6.28318530717959E+00,
      1.00000000000000E+01,
      1.50000000000000E+01,
      1.25663706143593E+01,
      2.00000000000000E+01,
      2.51327412287183E+01,
      3.14159265358979E+01,
      6.28318530717959E+01);

    constant CORRECT_ANSWERS : REALX_VECTOR(1 to ARG1'length) := (

      -1.00000000000000E+00,
      -1.00000000000000E+00,
      -1.00000000000000E+00,
      -1.00000000000000E+00,
      -9.99999999999813E-01,
      -9.99999999975677E-01,
      -9.99999995877693E-01,
      -9.99993025339611E-01,
      -9.99909204262595E-01,
      -9.96272076220750E-01,
      -9.64027580075817E-01,
      -9.56237458127739E-01,
      -9.46806012846268E-01,
      -9.35409070603099E-01,
      -9.21668554406471E-01,
      -9.17152335667273E-01,
      -9.05148253644866E-01,
      -8.85351648202263E-01,
      -8.61723159313306E-01,
      -8.33654607012155E-01,
      -7.61594155955765E-01,
      -7.16297870199025E-01,
      -6.64036770267849E-01,
      -6.55794202632672E-01,
      -6.04367777117164E-01,
      -5.37049566998035E-01,
      -4.62117157260010E-01,
      -3.79948962255225E-01,
      -3.73684747901215E-01,
      -2.91312612451591E-01,
      -1.97375320224904E-01,
      -1.93864558127089E-01,
      -9.96679946249558E-02,
      0.00000000000000E+00,
      9.96679946249558E-02,
      1.93864558127089E-01,
      1.97375320224904E-01,
      2.91312612451591E-01,
      3.73684747901215E-01,
      3.79948962255225E-01,
      4.62117157260010E-01,
      5.37049566998035E-01,
      6.04367777117164E-01,
      6.55794202632672E-01,
      6.64036770267849E-01,
      7.16297870199025E-01,
      7.61594155955765E-01,
      8.00499021760630E-01,
      8.33654607012155E-01,
      8.61723159313306E-01,
      8.85351648202263E-01,
      9.05148253644866E-01,
      9.17152335667273E-01,
      9.21668554406471E-01,
      9.35409070603099E-01,
      9.46806012846268E-01,
      9.56237458127739E-01,
      9.64027580075817E-01,
      9.96272076220750E-01,
      9.99909204262595E-01,
      9.99993025339611E-01,
      9.99999995877693E-01,
      9.99999999999813E-01,
      9.99999999975677E-01,
      1.00000000000000E+00,
      1.00000000000000E+00,
      1.00000000000000E+00,
      1.00000000000000E+00);

    variable RESULTS  : REALX_VECTOR(1 to ARG1'length);
    variable ACCURACY : INT_VECTOR(1 to ARG1'length);

  begin

    wait for 28 ns;

--      compute results

    for I in 1 to ARG1'length loop
      RESULTS(I) := TANH(ARG1(I));
    end loop;

--      compute accuracy

    for I in 1 to ARG1'length loop
      ACCURACY(I) := COMPARE(CORRECT_ANSWERS(I), RESULTS(I));
    end loop;

--      print results

    PRINT_RESULTS("TANH", ARG1, RESULTS, CORRECT_ANSWERS, ACCURACY, FULL_RESULTS);

    wait;

  end process TANH_TEST;


  ARCSINH_TEST : process

    constant ARG1 : REALX_VECTOR := (
      -6.28318530717959E+01,
      -3.14159265358979E+01,
      -2.51327412287183E+01,
      -1.25663706143593E+01,
      -1.00000000000000E+01,
      -9.50000000000000E+00,
      -9.00000000000000E+00,
      -8.50000000000000E+00,
      -8.00000000000000E+00,
      -7.50000000000000E+00,
      -7.00000000000000E+00,
      -6.50000000000000E+00,
      -6.28318530717959E+00,
      -6.00000000000000E+00,
      -5.50000000000000E+00,
      -5.00000000000000E+00,
      -4.50000000000000E+00,
      -4.00000000000000E+00,
      -3.50000000000000E+00,
      -3.14159265358979E+00,
      -3.00000000000000E+00,
      -2.50000000000000E+00,
      -2.00000000000000E+00,
      -1.57079632679489E+00,
      -1.50000000000000E+00,
      -1.00000000000000E+00,
      -7.85398163397448E-01,
      -5.00000000000000E-01,
      -3.92699081698724E-01,
      -1.96349540849362E-01,
      0.00000000000000E+00,
      1.96349540849362E-01,
      3.92699081698724E-01,
      5.00000000000000E-01,
      7.85398163397448E-01,
      1.00000000000000E+00,
      1.50000000000000E+00,
      1.57079632679489E+00,
      2.00000000000000E+00,
      2.50000000000000E+00,
      3.00000000000000E+00,
      3.14159265358979E+00,
      3.50000000000000E+00,
      4.00000000000000E+00,
      4.50000000000000E+00,
      5.00000000000000E+00,
      5.50000000000000E+00,
      6.00000000000000E+00,
      6.28318530717959E+00,
      6.50000000000000E+00,
      7.00000000000000E+00,
      7.50000000000000E+00,
      8.00000000000000E+00,
      8.50000000000000E+00,
      9.00000000000000E+00,
      9.50000000000000E+00,
      1.00000000000000E+01,
      1.25663706143593E+01,
      2.51327412287183E+01,
      3.14159265358979E+01,
      6.28318530717959E+01);

    constant CORRECT_ANSWERS : REALX_VECTOR(1 to ARG1'length) := (

      -4.83367265968874E+00,
      -4.14071536617305E+00,
      -3.91771415919954E+00,
      -3.22575082468029E+00,
      -2.99822295029797E+00,
      -2.94719762257003E+00,
      -2.89344398588587E+00,
      -2.83665572896893E+00,
      -2.77647228072372E+00,
      -2.71246530518434E+00,
      -2.64412076105863E+00,
      -2.57081467809570E+00,
      -2.53729750137336E+00,
      -2.49177985264491E+00,
      -2.40605912529802E+00,
      -2.31243834127275E+00,
      -2.20934770861533E+00,
      -2.09471254726110E+00,
      -1.96572047164965E+00,
      -1.86229574331085E+00,
      -1.81844645923207E+00,
      -1.64723114637110E+00,
      -1.44363547517881E+00,
      -1.23340311751121E+00,
      -1.19476321728711E+00,
      -8.81373587019543E-01,
      -7.21225488726780E-01,
      -4.81211825059604E-01,
      -3.83248085039393E-01,
      -1.95109290972995E-01,
      0.00000000000000E+00,
      1.95109290972995E-01,
      3.83248085039393E-01,
      4.81211825059604E-01,
      7.21225488726780E-01,
      8.81373587019543E-01,
      1.19476321728711E+00,
      1.23340311751121E+00,
      1.44363547517881E+00,
      1.64723114637110E+00,
      1.81844645923207E+00,
      1.86229574331085E+00,
      1.96572047164965E+00,
      2.09471254726110E+00,
      2.20934770861533E+00,
      2.31243834127275E+00,
      2.40605912529802E+00,
      2.49177985264491E+00,
      2.53729750137336E+00,
      2.57081467809570E+00,
      2.64412076105863E+00,
      2.71246530518434E+00,
      2.77647228072372E+00,
      2.83665572896893E+00,
      2.89344398588587E+00,
      2.94719762257003E+00,
      2.99822295029797E+00,
      3.22575082468029E+00,
      3.91771415919954E+00,
      4.14071536617305E+00,
      4.83367265968874E+00);

    variable RESULTS  : REALX_VECTOR(1 to ARG1'length);
    variable ACCURACY : INT_VECTOR(1 to ARG1'length);

  begin

    wait for 29 ns;

--      compute results

    for I in 1 to ARG1'length loop
      RESULTS(I) := ARCSINH(ARG1(I));
    end loop;

--      compute accuracy

    for I in 1 to ARG1'length loop
      ACCURACY(I) := COMPARE(CORRECT_ANSWERS(I), RESULTS(I));
    end loop;

--      print results

    PRINT_RESULTS("ARCSINH", ARG1, RESULTS, CORRECT_ANSWERS, ACCURACY, FULL_RESULTS);

    wait;

  end process ARCSINH_TEST;


  ARCCOSH_TEST : process

    constant ARG1 : REALX_VECTOR := (

      1.00000000000000E+00,
      1.50000000000000E+00,
      1.57079632679489E+00,
      2.00000000000000E+00,
      2.50000000000000E+00,
      3.00000000000000E+00,
      3.14159265358979E+00,
      3.50000000000000E+00,
      4.00000000000000E+00,
      4.50000000000000E+00,
      5.00000000000000E+00,
      5.50000000000000E+00,
      6.00000000000000E+00,
      6.28318530717959E+00,
      6.50000000000000E+00,
      7.00000000000000E+00,
      7.50000000000000E+00,
      8.00000000000000E+00,
      8.50000000000000E+00,
      9.00000000000000E+00,
      9.50000000000000E+00,
      1.00000000000000E+01,
      1.25663706143593E+01,
      2.51327412287183E+01,
      3.14159265358979E+01,
      6.28318530717959E+01,
      1.00000000000000E+02,
      3.00000000000000E+02,
      5.00000000000000E+02,
      1.00000000000000E+03,
      3.00000000000000E+03,
      5.00000000000000E+03,
      1.00000000000000E+05,
      3.00000000000000E+05,
      5.00000000000000E+05,
      1.00000000000000E+10);

    constant CORRECT_ANSWERS : REALX_VECTOR(1 to ARG1'length) := (

      0.00000000000000E+00,
      9.62423650119207E-01,
      1.02322747854755E+00,
      1.31695789692482E+00,
      1.56679923697241E+00,
      1.76274717403909E+00,
      1.81152627246085E+00,
      1.92484730023841E+00,
      2.06343706889556E+00,
      2.18464379160511E+00,
      2.29243166956118E+00,
      2.38952643457422E+00,
      2.47788873028848E+00,
      2.52463065993347E+00,
      2.55897897702861E+00,
      2.63391579384963E+00,
      2.70357583093140E+00,
      2.76865938331357E+00,
      2.82973503752439E+00,
      2.88727095035762E+00,
      2.94165731465119E+00,
      2.99322284612638E+00,
      3.22258451123829E+00,
      3.91692258703901E+00,
      4.14020876014649E+00,
      4.83354600820749E+00,
      5.29829236561049E+00,
      6.39692687742679E+00,
      6.90775427898064E+00,
      7.60090220954199E+00,
      8.69951472043241E+00,
      9.21034036197618E+00,
      1.22060726455052E+01,
      1.33046849341955E+01,
      1.38155105579633E+01,
      2.37189981105004E+01);


    variable RESULTS  : REALX_VECTOR(1 to ARG1'length);
    variable ACCURACY : INT_VECTOR(1 to ARG1'length);

  begin

    wait for 30 ns;

--      compute results

    for I in 1 to ARG1'length loop
      RESULTS(I) := ARCCOSH(ARG1(I));
    end loop;

--      compute accuracy

    for I in 1 to ARG1'length loop
      ACCURACY(I) := COMPARE(CORRECT_ANSWERS(I), RESULTS(I));
    end loop;

--      print results

    PRINT_RESULTS("ARCCOSH", ARG1, RESULTS, CORRECT_ANSWERS, ACCURACY, FULL_RESULTS);

    wait;

  end process ARCCOSH_TEST;

  ARCTANH_TEST : process


    constant ARG1 : REALX_VECTOR := (
      -9.99999900000000E-01,
      -9.99990000000000E-01,
      -9.99000000000000E-01,
      -9.90000000000000E-01,
      -9.80000000000000E-01,
      -9.70000000000000E-01,
      -9.60000000000000E-01,
      -9.50000000000000E-01,
      -9.00000000000000E-01,
      -8.50000000000000E-01,
      -8.00000000000000E-01,
      -7.85398163397448E-01,
      -7.50000000000000E-01,
      -7.00000000000000E-01,
      -6.50000000000000E-01,
      -6.00000000000000E-01,
      -5.50000000000000E-01,
      -5.00000000000000E-01,
      -4.50000000000000E-01,
      -4.00000000000000E-01,
      -3.92699081698724E-01,
      -3.50000000000000E-01,
      -3.00000000000000E-01,
      -2.50000000000000E-01,
      -2.00000000000000E-01,
      -1.96349540849362E-01,
      -1.90000000000000E-01,
      -1.80000000000000E-01,
      -1.70000000000000E-01,
      -1.60000000000000E-01,
      -1.50000000000000E-01,
      -1.40000000000000E-01,
      -1.30000000000000E-01,
      -1.20000000000000E-01,
      -1.10000000000000E-01,
      -1.00000000000000E-01,
      0.00000000000000E+00,
      1.00000000000000E-01,
      1.10000000000000E-01,
      1.20000000000000E-01,
      1.30000000000000E-01,
      1.40000000000000E-01,
      1.50000000000000E-01,
      1.60000000000000E-01,
      1.70000000000000E-01,
      1.80000000000000E-01,
      1.90000000000000E-01,
      1.96349540849362E-01,
      2.00000000000000E-01,
      2.50000000000000E-01,
      3.00000000000000E-01,
      3.50000000000000E-01,
      3.92699081698724E-01,
      4.00000000000000E-01,
      4.50000000000000E-01,
      5.00000000000000E-01,
      5.50000000000000E-01,
      6.00000000000000E-01,
      6.50000000000000E-01,
      7.00000000000000E-01,
      7.50000000000000E-01,
      7.85398163397448E-01,
      8.00000000000000E-01,
      8.50000000000000E-01,
      9.00000000000000E-01,
      9.50000000000000E-01,
      9.60000000000000E-01,
      9.70000000000000E-01,
      9.80000000000000E-01,
      9.90000000000000E-01,
      9.99000000000000E-01,
      9.99990000000000E-01,
      9.99999900000000E-01);

    constant CORRECT_ANSWERS : REALX_VECTOR(1 to ARG1'length) := (

      -8.40562139075913E+00,
      -6.10303382275884E+00,
      -3.80020116725020E+00,
      -2.64665241236225E+00,
      -2.29755992506729E+00,
      -2.09229572003494E+00,
      -1.94591014905531E+00,
      -1.83178082306482E+00,
      -1.47221948958322E+00,
      -1.25615281198806E+00,
      -1.09861228866811E+00,
      -1.05930617082324E+00,
      -9.72955074527657E-01,
      -8.67300527694053E-01,
      -7.75298706205584E-01,
      -6.93147180559945E-01,
      -6.18381313574464E-01,
      -5.49306144334055E-01,
      -4.84700278594052E-01,
      -4.23648930193602E-01,
      -4.14987256841989E-01,
      -3.65443754271396E-01,
      -3.09519604203112E-01,
      -2.55412811882995E-01,
      -2.02732554054082E-01,
      -1.98932863850063E-01,
      -1.92337169219545E-01,
      -1.81982688600706E-01,
      -1.71666663500579E-01,
      -1.61386696131526E-01,
      -1.51140435936467E-01,
      -1.40925576070494E-01,
      -1.30739850028878E-01,
      -1.20581028408444E-01,
      -1.10446915790097E-01,
      -1.00335347731076E-01,
      0.00000000000000E+00,
      1.00335347731076E-01,
      1.10446915790097E-01,
      1.20581028408444E-01,
      1.30739850028878E-01,
      1.40925576070494E-01,
      1.51140435936467E-01,
      1.61386696131526E-01,
      1.71666663500579E-01,
      1.81982688600706E-01,
      1.92337169219545E-01,
      1.98932863850063E-01,
      2.02732554054082E-01,
      2.55412811882995E-01,
      3.09519604203112E-01,
      3.65443754271396E-01,
      4.14987256841989E-01,
      4.23648930193602E-01,
      4.84700278594052E-01,
      5.49306144334055E-01,
      6.18381313574464E-01,
      6.93147180559945E-01,
      7.75298706205584E-01,
      8.67300527694053E-01,
      9.72955074527657E-01,
      1.05930617082324E+00,
      1.09861228866811E+00,
      1.25615281198806E+00,
      1.47221948958322E+00,
      1.83178082306482E+00,
      1.94591014905531E+00,
      2.09229572003494E+00,
      2.29755992506729E+00,
      2.64665241236225E+00,
      3.80020116725020E+00,
      6.10303382275884E+00,
      8.40562139075913E+00);


    variable RESULTS  : REALX_VECTOR(1 to ARG1'length);
    variable ACCURACY : INT_VECTOR(1 to ARG1'length);

  begin

    wait for 31 ns;

--      compute results

    for I in 1 to ARG1'length loop
      RESULTS(I) := ARCTANH(ARG1(I));
    end loop;

--      compute accuracy

    for I in 1 to ARG1'length loop
      ACCURACY(I) := COMPARE(CORRECT_ANSWERS(I), RESULTS(I));
    end loop;

--      print results

    PRINT_RESULTS("ARCTANH", ARG1, RESULTS, CORRECT_ANSWERS, ACCURACY, FULL_RESULTS);

    report "MATH_REAL test completed" severity note;
    wait;

  end process ARCTANH_TEST;

end architecture ARCH;

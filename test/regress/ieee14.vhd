-------------- test case header    ------
--!  Test intent :  Coverage of standards.
--!  Test scope  :  abs  a1  Nul  input.
--!  Keywords    : [operations, abs]
--!  References  : [VH2008 16.6]
--!                [Rlink : REQ08xx]
-----------------------------------------------
-- tb1 c1 - c30  from bishop tests.

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity ieee14 is
  generic (
    quiet : boolean := false);  -- make the simulation quiet
end entity;

architecture ops of ieee14 is
  signal ANULL: UNSIGNED (0 downto 1);
  signal BNULL: UNSIGNED (0 downto 1);
  signal SANULL: SIGNED (0 downto 1);
  signal SBNULL: SIGNED (0 downto 1);

begin
  process
    -- for c.28,c.30
    constant min  : Integer := -128;
    constant max  : Integer := 127;
    constant smax : Natural := 8;
    variable i    : Integer;
    variable s    : Signed( smax-1 downto 0 );

    variable sint : integer := 2;
    variable snat : natural := 2;

  begin
--  std c.29
    assert not(unsigned'("1000")  = snat) report "Test C.29 fail" severity failure;
    assert unsigned'("0010")       = snat report "Test C.29 fail" severity failure;
    assert not(unsigned'("00001") = snat) report "Test C.29 fail" severity failure;
    snat := 50000000;
    assert not(unsigned'("011111111") = snat) report "Test C.29 fail" severity failure;

    assert not(ANULL = snat) report "Test C.29 Null fail" severity failure;
    assert not(unsigned'("XXXXXX") = snat) report "Test C.29 X fail" severity failure;

    assert quiet report "C.29 tests done" severity note;

-- STD_C.1 tests:

    assert UNSIGNED'("1010") > UNSIGNED'("01")
      report "Test STD_C.1.2 failing."
      severity FAILURE;

    assert not (UNSIGNED'("010") > UNSIGNED'("010"))
      report "Test STD_C.1.5 failing."
      severity FAILURE;
    assert not(UNSIGNED'("10010") > UNSIGNED'("00010010"))
      report "Test STD_C.1.8 failing."
      severity FAILURE;

    assert not (UNSIGNED'("100010") > UNSIGNED'("110001"))
      report "Test STD_C.1.4 failing."
      severity FAILURE;
    assert not(UNSIGNED'("0001010") > UNSIGNED'("1111"))
      report "Test STD_C.1.6 test failing."
      severity FAILURE;
    assert not(UNSIGNED'("11") > UNSIGNED'("10001000"))
      report "Test STD_C.1.7 test failing."
      severity FAILURE;

    assert not(UNSIGNED'("011") > UNSIGNED'("110"))
      report "Test STD_C.1.10 failing."
      severity FAILURE;
    assert not(UNSIGNED'("10010") > UNSIGNED'("10010010"))
      report "Test STD_C.1.11 failing."
      severity FAILURE;
    assert not(UNSIGNED'("0010") > UNSIGNED'("110"))
      report "Test STD_C.1.12 failing."
      severity FAILURE;

    if (not quiet) then
      assert not (BNULL > UNSIGNED'("110"))
        report "Test STD_C.1.13 failing."
        severity FAILURE;
      assert not (ANULL > BNULL)
        report "Test STD_C.1.14 failing."
        severity FAILURE;
    end if;


    assert quiet report "C.1 tests done" severity note;

--STD_C.2 tests:

    assert SIGNED'("0010") > SIGNED'("0001")
      report "Test STD_C.2.1 failing."
      severity FAILURE;
    assert SIGNED'("01010") > SIGNED'("01")
      report "Test STD_C.2.2 failing."
      severity FAILURE;
    assert SIGNED'("0111") > SIGNED'("000001")
      report "Test STD_C.2.3 failing."
      severity FAILURE;
    assert SIGNED'("011100") > SIGNED'("111100")
      report "Test STD_C.2.16 failing."
      severity FAILURE;

    assert not (SIGNED'("010") > SIGNED'("010"))
      report "Test STD_C.2.5 failing."
      severity FAILURE;
    assert SIGNED'("111110010") > SIGNED'("10000010")
      report "Test STD_C.2.8 failing."
      severity FAILURE;
    assert SIGNED'("0010") > SIGNED'("10")
      report "Test STD_C.2.9 failing."
      severity FAILURE;
    assert not(SIGNED'("0000") > SIGNED'("00"))
      report "Test STD_C.2.17 failing."
      severity FAILURE;
    assert SIGNED'("0000") > SIGNED'("11100001100")
      report "Test STD_C.2.18 failing."
      severity FAILURE;

    assert not (SIGNED'("100010") > SIGNED'("110001"))
      report "Test STD_C.2.4 failing."
      severity FAILURE;
    assert not(SIGNED'("1001010") > SIGNED'("1111"))
      report "Test STD_C.2.6 test failing."
      severity FAILURE;
    assert not(SIGNED'("10001000") > SIGNED'("11"))
      report "Test STD_C.2.7 test failing."
      severity FAILURE;

    assert not(SIGNED'("110") > SIGNED'("010"))
      report "Test STD_C.2.10 failing."
      severity FAILURE;
    assert not(SIGNED'("100100010") > SIGNED'("1001"))
      report "Test STD_C.2.11 failing."
      severity FAILURE;
    assert not(SIGNED'("10010") > SIGNED'("110"))
      report "Test STD_C.2.12 failing."
      severity FAILURE;
    assert not(SIGNED'("0000") > SIGNED'("00000110"))
      report "Test STD_C.2.20 failing."
      severity FAILURE;

    if (not quiet) then
      assert not (SBNULL > SIGNED'("110"))
        report "Test STD_C.2.13 failing."
        severity FAILURE;
      assert not (SANULL > SBNULL)
        report "Test STD_C.2.14 failing."
        severity FAILURE;
    end if;
    assert quiet report "C.2 tests done" severity note;

-- STD_C.3 tests

    assert 2 > UNSIGNED'("0001")
      report "Test STD_C.3.1 failing."
      severity FAILURE;
    assert 10 > UNSIGNED'("01")
      report "Test STD_C.3.2 failing."
      severity FAILURE;
    assert 7 > UNSIGNED'("000001")
      report "Test STD_C.3.3 failing."
      severity FAILURE;

    assert not (2 > UNSIGNED'("010"))
      report "Test STD_C.3.5 failing."
      severity FAILURE;
    assert not(18 > UNSIGNED'("00010010"))
      report "Test STD_C.3.8 failing."
      severity FAILURE;

    assert not (34 > UNSIGNED'("110001"))
      report "Test STD_C.3.4 failing."
      severity FAILURE;
    assert not(10 > UNSIGNED'("1111"))
      report "Test STD_C.3.6 test failing."
      severity FAILURE;
    assert not(3 > UNSIGNED'("10001000"))
      report "Test STD_C.3.7 test failing."
      severity FAILURE;

    assert not(3 > UNSIGNED'("110"))
      report "Test STD_C.3.10 failing."
      severity FAILURE;
    assert not(18 > UNSIGNED'("10010010"))
      report "Test STD_C.3.11 failing."
      severity FAILURE;
    assert not(2 > UNSIGNED'("110"))
      report "Test STD_C.3.12 failing."
      severity FAILURE;

    assert (10000 > UNSIGNED'("110"))
      report "Test STD_C.3.13 failing."
      severity FAILURE;
    if (not quiet) then
      assert not ( 0 > BNULL)
        report "Test STD_C.3.14 failing."
        severity FAILURE;
    end if;

    assert quiet report "C.3 tests done" severity note;

-- STD_C.4 tests

    assert 10 > SIGNED'("01")
      report "Test STD_C.4.2 failing."
      severity FAILURE;
    assert 7 > SIGNED'("100001")
      report "Test STD_C.4.3 failing."
      severity FAILURE;
    assert not (-4 > SIGNED'("111100"))
      report "Test STD_C.4.15 failing."
      severity FAILURE;
    assert 28 > SIGNED'("111100")
      report "Test STD_C.4.16 failing."
      severity FAILURE;

    assert not(2 > SIGNED'("010"))
      report "Test STD_C.4.5 failing."
      severity FAILURE;
    assert -14 > SIGNED'("100010010")
      report "Test STD_C.4.8 failing."
      severity FAILURE;
    assert 2 > SIGNED'("10")
      report "Test STD_C.4.9 failing."
      severity FAILURE;
    assert not(0 > SIGNED'("00"))
      report "Test STD_C.4.17 failing."
      severity FAILURE;
    assert -0 > SIGNED'("11100001100")
      report "Test STD_C.4.18 failing."
      severity FAILURE;

    assert not (-30 > SIGNED'("110001"))
      report "Test STD_C.4.4 failing."
      severity FAILURE;
    assert not(+10 > SIGNED'("01111"))
      report "Test STD_C.4.6 test failing."
      severity FAILURE;
    assert not(-9 > SIGNED'("1100"))
      report "Test STD_C.4.7 test failing."
      severity FAILURE;
    assert not(3 > SIGNED'("010001000"))
      report "Test STD_C.4.19 test failing."
      severity FAILURE;

    assert not(-15 > SIGNED'("110"))
      report "Test STD_C.4.11 failing."
      severity FAILURE;
    assert not(2 > SIGNED'("0110"))
      report "Test STD_C.4.12 failing."
      severity FAILURE;
    assert not(0 > SIGNED'("00000110"))
      report "Test STD_C.4.20 failing."
      severity FAILURE;

    assert not (0 > SIGNED'("0110"))
      report "Test STD_C.4.13 failing."
      severity FAILURE;
    assert not (-1000000 > SIGNED'("110"))
      report "Test STD_C.4.14 failing."
      severity FAILURE;


    assert quiet report "C.4 tests done" severity note;

-- STD_C.5 tests

    assert UNSIGNED'("0010") > 1
      report "Test STD_C.5.1 failing."
      severity FAILURE;
    assert UNSIGNED'("1010") > 1
      report "Test STD_C.5.2 failing."
      severity FAILURE;
    assert UNSIGNED'("111") > 1
      report "Test STD_C.5.3 failing."
      severity FAILURE;

    assert not(UNSIGNED'("010") > 2)
      report "Test STD_C.5.5 failing."
      severity FAILURE;
    assert not(UNSIGNED'("10010") > 18)
      report "Test STD_C.5.8 failing."
      severity FAILURE;
    assert not(UNSIGNED'("0010") > 2)
      report "Test STD_C.5.9 failing."
      severity FAILURE;

    assert not (UNSIGNED'("100010") > 49)
      report "Test STD_C.5.4 failing."
      severity FAILURE;
    assert not(UNSIGNED'("0001010") > 15)
      report "Test STD_C.5.6 test failing."
      severity FAILURE;
    assert not(UNSIGNED'("11") > 151)
      report "Test STD_C.5.7 test failing."
      severity FAILURE;

    assert not(UNSIGNED'("10010") > 256)
      report "Test STD_C.5.11 failing."
      severity FAILURE;
    assert not(UNSIGNED'("0010") > 6)
      report "Test STD_C.5.12 failing."
      severity FAILURE;

    if (not quiet) then
      assert not (BNULL > 8)
        report "Test STD_C.5.13 failing."
        severity FAILURE;
    end if;
    assert not (UNSIGNED'("111") > 3267)
      report "Test STD_C.5.14 failing."
      severity FAILURE;

    assert quiet report "C.5 tests done" severity note;

-- STD_C.6 tests:

    assert SIGNED'("0010") > 1
      report "Test STD_C.6.1 failing."
      severity FAILURE;
    assert SIGNED'("01010") > 8
      report "Test STD_C.6.2 failing."
      severity FAILURE;
    assert SIGNED'("0111") > 1
      report "Test STD_C.6.3 failing."
      severity FAILURE;
    assert SIGNED'("011100") > -4
      report "Test STD_C.6.16 failing."
      severity FAILURE;

    assert not(SIGNED'("010") > 2)
      report "Test STD_C.6.5 failing."
      severity FAILURE;
    assert not(SIGNED'("10010") > -14)
      report "Test STD_C.6.8 failing."
      severity FAILURE;
    assert SIGNED'("0010") > -2
      report "Test STD_C.6.9 failing."
      severity FAILURE;
    assert not(SIGNED'("0000") > 0)
      report "Test STD_C.6.17 failing."
      severity FAILURE;
    assert SIGNED'("0000") > -2048
      report "Test STD_C.6.18 failing."
      severity FAILURE;

    assert not(SIGNED'("10001010") > 1)
      report "Test STD_C.6.6 test failing."
      severity FAILURE;
    assert not(SIGNED'("10011100111") > -345)
      report "Test STD_C.6.7 test failing."
      severity FAILURE;
    assert not(SIGNED'("1000000111111") > -1024)
      report "Test STD_C.6.19 test failing."
      severity FAILURE;

    assert not(SIGNED'("1000011") > -2)
      report "Test STD_C.6.10 failing."
      severity FAILURE;
    assert not(SIGNED'("10000010") > -63)
      report "Test STD_C.6.11 failing."
      severity FAILURE;
    assert not(SIGNED'("1000") > 0)
      report "Test STD_C.6.20 failing."
      severity FAILURE;

    if (not quiet) then
      assert not (SBNULL > -7)
        report "Test STD_C.6.13 failing."
        severity FAILURE;
    end if;
    assert quiet report "C.6 tests done" severity note;

-- STD_C.7 tests:

    assert UNSIGNED'("0001") < UNSIGNED'("0010")
      report "Test STD_C.7.1 failing."
      severity FAILURE;
    assert UNSIGNED'("01") < UNSIGNED'("1010")
      report "Test STD_C.7.2 failing."
      severity FAILURE;
    assert UNSIGNED'("000001") < UNSIGNED'("111")
      report "Test STD_C.7.3 failing."
      severity FAILURE;

    assert not(UNSIGNED'("010") < UNSIGNED'("010"))
      report "Test STD_C.7.5 failing."
      severity FAILURE;
    assert not(UNSIGNED'("00010010") < UNSIGNED'("10010"))
      report "Test STD_C.7.8 failing."
      severity FAILURE;
    assert not(UNSIGNED'("10") < UNSIGNED'("0010"))
      report "Test STD_C.7.9 failing."
      severity FAILURE;

    assert not (UNSIGNED'("110001") < UNSIGNED'("100010"))
      report "Test STD_C.7.4 failing."
      severity FAILURE;
    assert not(UNSIGNED'("1111") < UNSIGNED'("0001010"))
      report "Test STD_C.7.6 test failing."
      severity FAILURE;
    assert not(UNSIGNED'("10001000") < UNSIGNED'("11"))
      report "Test STD_C.7.7 test failing."
      severity FAILURE;

    assert not(UNSIGNED'("110") < UNSIGNED'("011"))
      report "Test STD_C.7.10 failing."
      severity FAILURE;
    assert not(UNSIGNED'("10010010") < UNSIGNED'("10010"))
      report "Test STD_C.7.11 failing."
      severity FAILURE;

    if (not quiet) then
      assert not ( UNSIGNED'("110") < BNULL)
        report "Test STD_C.7.13 failing."
        severity FAILURE;
      assert not (BNULL < ANULL)
        report "Test STD_C.7.14 failing."
        severity FAILURE;
    end if;

    assert quiet report "C.7 tests done" severity note;

--STD_C.8 tests:

    assert SIGNED'("0001") < SIGNED'("0010")
      report "Test STD_C.8.1 failing."
      severity FAILURE;
    assert SIGNED'("01") < SIGNED'("01010")
      report "Test STD_C.8.2 failing."
      severity FAILURE;
    assert SIGNED'("000001") < SIGNED'("0111")
      report "Test STD_C.8.3 failing."
      severity FAILURE;
    assert not(SIGNED'("111100") < SIGNED'("11100"))
      report "Test STD_C.8.15 failing."
      severity FAILURE;
    assert SIGNED'("111100") < SIGNED'("011100")
      report "Test STD_C.8.16 failing."
      severity FAILURE;

    assert not(SIGNED'("010") < SIGNED'("010"))
      report "Test STD_C.8.5 failing."
      severity FAILURE;
    assert SIGNED'("10000010") < SIGNED'("111110010")
      report "Test STD_C.8.8 failing."
      severity FAILURE;
    assert SIGNED'("10") < SIGNED'("0010")
      report "Test STD_C.8.9 failing."
      severity FAILURE;
    assert not(SIGNED'("00") < SIGNED'("0000"))
      report "Test STD_C.8.17 failing."
      severity FAILURE;
    assert SIGNED'("11100001100") < SIGNED'("0000")
      report "Test STD_C.8.18 failing."
      severity FAILURE;

    assert not (SIGNED'("110001") < SIGNED'("100010"))
      report "Test STD_C.8.4 failing."
      severity FAILURE;
    assert not(SIGNED'("1111") < SIGNED'("1001010"))
      report "Test STD_C.8.6 test failing."
      severity FAILURE;
    assert not(SIGNED'("11") < SIGNED'("10001000"))
      report "Test STD_C.8.7 test failing."
      severity FAILURE;
    assert not(SIGNED'("0110") < SIGNED'("10001000"))
      report "Test STD_C.8.19 test failing."
      severity FAILURE;

    assert not(SIGNED'("010") < SIGNED'("110"))
      report "Test STD_C.8.10 failing."
      severity FAILURE;
    assert not(SIGNED'("1001") < SIGNED'("100100010"))
      report "Test STD_C.8.11 failing."
      severity FAILURE;
    assert not(SIGNED'("110") < SIGNED'("10010"))
      report "Test STD_C.8.12 failing."
      severity FAILURE;
    assert not(SIGNED'("00000110") < SIGNED'("0000"))
      report "Test STD_C.8.20 failing."
      severity FAILURE;

    if (not quiet) then
      assert not (SBNULL < SANULL)
        report "Test STD_C.8.14 failing."
        severity FAILURE;
    end if;

    assert quiet report "C.8 tests done" severity note;

-- STD_C.11 tests

    assert UNSIGNED'("0001") < 2
      report "Test STD_C.11.1 failing."
      severity FAILURE;
    assert UNSIGNED'("01") < 10
      report "Test STD_C.11.2 failing."
      severity FAILURE;
    assert UNSIGNED'("000001") < 7
      report "Test STD_C.11.3 failing."
      severity FAILURE;

    assert not(UNSIGNED'("010") < 2)
      report "Test STD_C.11.5 failing."
      severity FAILURE;
    assert not(UNSIGNED'("00010010") < 18)
      report "Test STD_C.11.8 failing."
      severity FAILURE;
    assert not(UNSIGNED'("10") < 2)
      report "Test STD_C.11.9 failing."
      severity FAILURE;

    assert not (UNSIGNED'("110001") < 34)
      report "Test STD_C.11.4 failing."
      severity FAILURE;
    assert not(UNSIGNED'("1111") < 10)
      report "Test STD_C.11.6 test failing."
      severity FAILURE;
    assert not(UNSIGNED'("10001000") < 3)
      report "Test STD_C.11.7 test failing."
      severity FAILURE;

    assert not(UNSIGNED'("110") < 3)
      report "Test STD_C.11.10 failing."
      severity FAILURE;
    assert not(UNSIGNED'("10010010") < 18)
      report "Test STD_C.11.11 failing."
      severity FAILURE;
    assert not(UNSIGNED'("110") < 2)
      report "Test STD_C.11.12 failing."
      severity FAILURE;

    if (not quiet) then
      assert not ( BNULL < 0)
        report "Test STD_C.11.14 failing."
        severity FAILURE;
    end if;
    assert quiet report "C.11 tests done" severity note;

-- STD_C.12 tests

    assert SIGNED'("0001") < 2
      report "Test STD_C.12.1 failing."
      severity FAILURE;
    assert SIGNED'("01") < 10
      report "Test STD_C.12.2 failing."
      severity FAILURE;
    assert SIGNED'("100001") < 7
      report "Test STD_C.12.3 failing."
      severity FAILURE;
    assert not(SIGNED'("111100") < -4)
      report "Test STD_C.12.15 failing."
      severity FAILURE;
    assert SIGNED'("111100") < 28
      report "Test STD_C.12.16 failing."
      severity FAILURE;

    assert not(SIGNED'("010") < 2)
      report "Test STD_C.12.5 failing."
      severity FAILURE;
    assert SIGNED'("10") < 2
      report "Test STD_C.12.9 failing."
      severity FAILURE;
    assert not(SIGNED'("00") < 0)
      report "Test STD_C.12.17 failing."
      severity FAILURE;
    assert SIGNED'("11100001100") < -0
      report "Test STD_C.12.18 failing."
      severity FAILURE;

    assert not (SIGNED'("110001") < -30)
      report "Test STD_C.12.4 failing."
      severity FAILURE;
    assert not(SIGNED'("01111") < +10)
      report "Test STD_C.12.6 test failing."
      severity FAILURE;
    assert not(SIGNED'("1100") < -9)
      report "Test STD_C.12.7 test failing."
      severity FAILURE;

    assert not(SIGNED'("0110") < 3)
      report "Test STD_C.12.10 failing."
      severity FAILURE;
    assert not(SIGNED'("110") < -15)
      report "Test STD_C.12.11 failing."
      severity FAILURE;
    assert not(SIGNED'("0110") < 2)
      report "Test STD_C.12.12 failing."
      severity FAILURE;
    assert not(SIGNED'("00000110") < 0)
      report "Test STD_C.12.20 failing."
      severity FAILURE;

    assert not (SIGNED'("0110") < 0)
      report "Test STD_C.12.13 failing."
      severity FAILURE;
    assert not (SIGNED'("110") < -1000000)
      report "Test STD_C.12.14 failing."
      severity FAILURE;


    assert quiet report "C.12 tests done" severity note;

-- STD_C.9 tests

    assert 1 < UNSIGNED'("0010")
      report "Test STD_C.9.1 failing."
      severity FAILURE;
    assert 1 < UNSIGNED'("111")
      report "Test STD_C.9.3 failing."
      severity FAILURE;

    assert not(2 < UNSIGNED'("010"))
      report "Test STD_C.9.5 failing."
      severity FAILURE;
    assert not(18 < UNSIGNED'("10010"))
      report "Test STD_C.9.8 failing."
      severity FAILURE;
    assert not(2 < UNSIGNED'("0010"))
      report "Test STD_C.9.9 failing."
      severity FAILURE;

    assert not (49 < UNSIGNED'("100010"))
      report "Test STD_C.9.4 failing."
      severity FAILURE;
    assert not(15 < UNSIGNED'("0001010"))
      report "Test STD_C.9.6 test failing."
      severity FAILURE;
    assert not(151 < UNSIGNED'("11"))
      report "Test STD_C.9.7 test failing."
      severity FAILURE;

    assert not(256 < UNSIGNED'("10010"))
      report "Test STD_C.9.11 failing."
      severity FAILURE;
    assert not(6 < UNSIGNED'("0010"))
      report "Test STD_C.9.12 failing."
      severity FAILURE;

    if (not quiet) then
      assert not (8 < BNULL)
        report "Test STD_C.9.13 failing."
        severity FAILURE;
    end if;
    assert not (3267 < UNSIGNED'("111"))
      report "Test STD_C.9.14 failing."
      severity FAILURE;

    assert quiet report "C.9 tests done" severity note;

-- STD_C.10 tests:

    assert 1 < SIGNED'("0010")
      report "Test STD_C.10.1 failing."
      severity FAILURE;
    assert 1 < SIGNED'("0111")
      report "Test STD_C.10.3 failing."
      severity FAILURE;
    assert not(-4 < SIGNED'("11100"))
      report "Test STD_C.10.15 failing."
      severity FAILURE;
    assert -4 < SIGNED'("011100")
      report "Test STD_C.10.16 failing."
      severity FAILURE;

    assert not(2 < SIGNED'("010"))
      report "Test STD_C.10.5 failing."
      severity FAILURE;
    assert not(-14 < SIGNED'("10010"))
      report "Test STD_C.10.8 failing."
      severity FAILURE;
    assert -2 < SIGNED'("0010")
      report "Test STD_C.10.9 failing."
      severity FAILURE;
    assert not(0 < SIGNED'("0000"))
      report "Test STD_C.10.17 failing."
      severity FAILURE;
    assert -2048 < SIGNED'("0000")
      report "Test STD_C.10.18 failing."
      severity FAILURE;

    assert not (-15 < SIGNED'("100010"))
      report "Test STD_C.10.4 failing."
      severity FAILURE;
    assert not(1 < SIGNED'("10001010"))
      report "Test STD_C.10.6 test failing."
      severity FAILURE;
    assert not(-1024 < SIGNED'("1000000111111"))
      report "Test STD_C.10.19 test failing."
      severity FAILURE;

    assert not(-2 < SIGNED'("1000011"))
      report "Test STD_C.10.10 failing."
      severity FAILURE;
    assert not(-63 < SIGNED'("10000010"))
      report "Test STD_C.10.11 failing."
      severity FAILURE;
    assert not(0 < SIGNED'("1000"))
      report "Test STD_C.10.20 failing."
      severity FAILURE;

    if (not quiet) then
      assert not (-7 < SBNULL)
        report "Test STD_C.10.13 failing."
        severity FAILURE;
    end if;
    assert not (-6457 < SIGNED'("100000000000000010"))
      report "Test STD_C.10.14 failing."
      severity FAILURE;

    assert quiet report "C.10 tests done" severity note;

-- c.28, c.30 tests, from Wolfgang:

    for a in min to max loop
      i := a;
      s := To_Signed( i, smax );
      assert i = s
        report "failure in Function C.28 !"
        severity failure;
      assert s = i
        report "failure in Function C.30 !"
        severity failure;
    end loop;

    assert not(sint = sanull) report "Test C.28 Null fail" severity failure;
    assert not(sint = signed'("XXXXXX")) report "Test C.29  X's fail" severity failure;
    assert not(sanull = sint) report "Test C.30 Null fail" severity failure;
    assert not(signed'("XXXXXX") = sint) report "Test C.30 X's fail" severity failure;
    sint := 50000000;
    assert not(sint = signed'("01111111")) report "Test C.29 larger int fail" severity failure;
    assert not(signed'("01111111") = sint) report "Test C.30 larger int fail" severity failure;



    assert quiet report "c.28, c.30 tests done" severity note;

    assert FALSE
      report "END OF test suite tb3"
      severity note;

    wait;
  end process;

end ops;

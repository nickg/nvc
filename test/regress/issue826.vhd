library IEEE ;
use ieee.std_logic_1164.all ;
use ieee.numeric_std.all ;

entity issue826 is
end entity ;
architecture test of issue826 is
  constant ZERO : std_logic_vector(7 downto 0) := X"00" ;
  constant ZERO_UV : unsigned(0 to 7) := X"00" ;
begin
  TestProc : process
    variable SLV : std_logic_vector(7 downto 0) ;
    variable SLVR : std_logic_vector(0 to 7) ;
    variable UV : unsigned(0 to 7) ;
    variable SL : std_logic ;
  begin
    SLV := (others => '0') ;
    assert SLV = X"00" report to_hstring(SLV);

    SLV := (7 downto 0 => '0') ;
    assert SLV = X"00" report to_hstring(SLV);

    SLV := ('0','0','0','0','0','0','0','0') ;
    assert SLV = X"00" report to_hstring(SLV);

    SLV := ((7|6 =>'0', 5 downto 0 =>'0')) ;
    assert SLV = X"00" report to_hstring(SLV);

    SLV := (7|6 =>'1', 5 downto 0 =>'0') ;
    assert SLV = X"C0" report to_hstring(SLV);

    SLV := (0 to 5 =>'0', 6 =>'1',  7 =>'1') ;
    assert SLV = X"C0" report to_hstring(SLV);

    SLVR := (7|6 =>'1', 5 downto 0 =>'0') ;
    assert SLVR = X"03" report to_hstring(SLV);

    SLVR := (0 to 5 =>'0', 6 =>'1',  7 =>'1') ;
    assert SLVR = X"03" report to_hstring(SLV);

    SLV := not((0 to 5 =>'0', 6 =>'1',  7 =>'1')) ;
    assert SLV = X"FC" report to_hstring(SLV);

    UV := not((0 to 5 =>'0', 6 =>'1',  7 =>'1')) ;
    assert SLV = X"FC" report to_hstring(SLV);

    SLV := not((7 =>'1', 6 =>'1',  5 downto 0 =>'0')) ;
    assert SLV = X"FC" report to_hstring(SLV);

    UV := not((7 =>'1', 6 =>'1',  5 downto 0 =>'0')) ;
    assert SLV = X"FC" report to_hstring(SLV);

    SLV := ZERO or (7|6 =>'1', 5 downto 0 =>'0') ;
    assert SLV = X"03" report to_hstring(SLV);

    SLV := ZERO or (0 to 5 =>'0', 6 =>'1',  7 =>'1') ;
    assert SLV = X"03" report to_hstring(SLV);

    SLV := (0 to 5 =>'0', 6 =>'1',  7 =>'1') or ZERO ;
    assert SLV = X"03" report to_hstring(SLV);

    UV := ZERO_UV or (7|6 =>'1', 5 downto 0 =>'0') ;
    assert UV = X"03" report to_hstring(SLV);

    UV := ZERO_UV or (0 to 5 =>'0', 6 =>'1',  7 =>'1') ;
    assert UV = X"03" report to_hstring(SLV);

    UV := (0 to 5 =>'0', 6 =>'1',  7 =>'1') or ZERO_UV ;
    assert UV = X"03" report to_hstring(SLV);

    UV := ZERO_UV + (7|6 =>'1', 5 downto 0 =>'0') ;
    assert UV = X"03" report to_hstring(SLV);

    UV := ZERO_UV + (0 to 5 =>'0', 6 =>'1',  7 =>'1') ;
    assert UV = X"03" report to_hstring(SLV);

    UV := (0 to 5 =>'0', 6 =>'1',  7 =>'1') + ZERO_UV ;
    assert UV = X"03" report to_hstring(SLV);

    SL := "not"(not(std_logic_vector'(0 to 5 =>'0', 6 =>'1',  7 =>'1')))(7) ;
    assert SL = '1';

    SL := "not"(not(unsigned'(0 to 5 =>'0', 6 =>'1',  7 =>'1')))(7) ;
    assert SL = '0';

    wait ;
  end process TestProc ;
end architecture test ;

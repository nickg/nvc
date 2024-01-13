library IEEE ;
use ieee.std_logic_1164.all ;
use ieee.numeric_std.all ;

entity issue827 is
end entity issue827 ;
architecture test of issue827 is

begin
  TestProc : process
    variable SLV : std_logic_vector(7 downto 0) ;
    variable UV : unsigned(0 to 7) ;
    variable SL : std_logic ;

    type t_byte_array is array (natural range <>) of std_logic_vector(7 downto 0);
    variable ba : t_byte_array(2 downto 0);
  begin
    SLV := (3 downto 0 => X"C", others => '0') ;
    assert SLV =  X"0C" report to_hstring(SLV);

    SLV := (7 downto 4 => '0') & X"C" ;
    assert SLV =  X"0C" report to_hstring(SLV);

    SLV := (4 to 7 => '0') & X"C" ;
    assert SLV =  X"0C" report to_hstring(SLV);

    wait ;

  end process TestProc ;
end architecture test ;

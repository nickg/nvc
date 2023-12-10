entity bounds21 is
end entity;

architecture test of bounds21 is

  type  boolean_v is array (integer range <>) of boolean;
  subtype boolean_4  is boolean_v (1 to 4);
  subtype boolean_7  is boolean_v (1 to 7);

  function return_array return boolean_4 is
    constant l_operand : boolean_4 := (true,false,true,false);
  begin
    return l_operand;
  end return_array;

begin
  l : block
    generic ( info : boolean_7 );
    generic map ( return_array & return_array );
  begin
    assert not(info = (true,false,true,false,true,false,true))
      report "***passed test: c07s02b04x00p20n01i02094"
      severity note;
    assert (info = (true,false,true,false,true,false,true))
      report "***failed test: c07s02b04x00p20n01i02094 - function array concatenation did not succeed."
      severity error;
  end block;

end architecture;

-- From VHDL Compliance-Tests repo
library ieee;
use ieee.numeric_std.all;

entity func24 is
end entity;

architecture tb of func24 is
begin
  main: process
    function meaning_of_life return return_value of unsigned is
    begin
      return to_unsigned(42, return_value'length);
    end function;

    variable the_answer : unsigned(7 downto 0);
  begin
    the_answer := meaning_of_life;
    assert the_answer = 42;
    wait;
  end process;
end architecture;

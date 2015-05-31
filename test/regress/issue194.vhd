package pkg is
  function other_fun return integer;

  function fun return integer;
end package;

package body pkg is
  function other_fun return integer is
  begin
    return 0;
  end function;

  function fun return integer is
    function nested return integer is
    begin
      return other_fun;
    end;
  begin
    return nested;
  end function;
end package body;

use work.pkg.all;

entity issue194 is
end entity;

architecture a of issue194 is
begin
  main : process
  begin
    assert fun = 0;
    wait;
  end process;
end architecture;

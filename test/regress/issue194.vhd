-- -*- mode: vhdl; vhdl-basic-offset: 2 -*-
package pkg is
  function other_fun(x : integer) return integer;

  function fun(x : integer) return integer;
end package;

package body pkg is
  function other_fun(x : integer) return integer is
  begin
    return x;
  end function;

  function fun(x : integer) return integer is
    function nested(x : integer) return integer is
    begin
      return other_fun(x);
    end;
  begin
    return nested(x);
  end function;
end package body;

use work.pkg.all;

entity issue194 is
end entity;

architecture a of issue194 is
begin
  main : process
    variable x : integer;
  begin
    x := 0;
    wait for 1 ns;
    assert fun(x) = 0;
    wait;
  end process;
end architecture;

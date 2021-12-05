package pkg is
  function func (s : string) return natural;
  function func2 (s : natural) return natural;
  function func3(x : integer) return integer;
end package;

package body pkg is
  function func(s : string) return natural is
    function inner_func return natural is
    begin
      return s'length;
    end;
  begin
    return inner_func;
  end;

  function func2(s : natural) return natural is
    function inner_func return natural is
    begin
      return s;
    end;
  begin
    return inner_func;
  end;

  function func3(x : integer) return integer is
      function inner(n : integer) return integer is
      begin
          return x + n;
      end function;
  begin
      return inner(2);
  end function;
end;

use work.pkg.all;

entity bug is
end entity;

architecture a of bug is
begin
  main : process
  begin
    s1: assert func("") = 0;
    s2: assert func("abc") = 3;
    s3: assert func2(10) = 10;
    s4: assert func3(5) = 7;
    wait;
  end process;
end;

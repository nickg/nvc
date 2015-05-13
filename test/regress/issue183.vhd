package pkg is
    function fun(constant s : string) return string;
    impure function ifun(constant s : string) return bit_vector;
end package;

package body pkg is

  function fun(constant s : string) return string is
    function nested_fun return integer is
    begin
      if s'length = 0 then
        return 1;
      else
        return s'length;
      end if;
    end function;
    variable ret_val : string(1 to nested_fun);
  begin
    return ret_val;
  end;

  impure function ifun(constant s : string) return bit_vector is
    -- Renaming to nested_fun2 makes it work
    function nested_fun return integer is
    begin
      if s'length = 0 then
        return 1;
      else
        return s'length;
      end if;
    end function;

    variable ret_val : bit_vector(1 to nested_fun);
  begin
    return ret_val;
  end;

end package body;

-------------------------------------------------------------------------------

entity issue183 is
end entity;

use work.pkg.all;

architecture test of issue183 is
begin

    process is
        variable s : string(1 to 2);
    begin
        assert fun(s) = s;
        assert ifun(s) = "00";
        wait;
    end process;

end architecture;

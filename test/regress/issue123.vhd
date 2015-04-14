package nested_function_bug is
  procedure proc(param : integer; result : out integer);
end package;

package body nested_function_bug is
  procedure proc(param : integer; result : out integer) is
    variable foo : bit_vector(0 to param);
    impure function nested_function return integer is
    begin
      return foo'length;
    end;
  begin
    result := nested_function;
  end;
end package body;

-------------------------------------------------------------------------------

entity issue123 is
end entity;

use work.nested_function_bug.all;

architecture test of issue123 is
begin

    process is
        variable result : integer;
    begin
        proc(5, result);
        assert result = 6;
        wait;
    end process;

end architecture;

package nested_function_bug is
  procedure proc(param : integer; result : out integer);
end package;

package body nested_function_bug is
  procedure proc(param : integer; result : out integer) is
    impure function nested_function return integer is
    begin
      return param * 2;
    end;
    variable bar : natural := nested_function;
  begin
      result := bar;
  end;
end package body;

-------------------------------------------------------------------------------

entity issue122 is
end entity;

use work.nested_function_bug.all;

architecture test of issue122 is
begin

    process is
        variable r : integer;
    begin
        proc(5, r);
        assert r = 10;
        wait;
    end process;

end architecture;

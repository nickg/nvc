package nested_pkg is
  procedure parent_proc(signal sig : out integer;
                        var : inout integer);
end package;

package body nested_pkg is
  procedure parent_proc(signal sig : out integer; var : inout integer) is
    procedure nested_proc is
    begin
        assert var /= 4;
        sig <= var;
        var := 4;
    end procedure;
  begin
    nested_proc;
  end procedure;
end package body;

entity issue82 is
end entity;

use work.nested_pkg.all;

architecture test of issue82 is
    signal s : integer;
begin

    process is
        variable v : integer := 2;
    begin
        parent_proc(s, v);
        wait for 1 ns;
        assert v = 4;
        assert s = 2;
        wait;
    end process;

end architecture;

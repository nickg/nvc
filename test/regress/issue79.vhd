package access_bug is
  type integer_access is access integer;
  type integer_access_array is array (natural range <>) of integer_access;
end package;

package body access_bug is
  function bug_function return integer_access_array is
    variable bug_here : integer_access_array(0 to 0);
  begin
    return bug_here;
  end function;
end package body;

entity issue79 is
end entity;

use work.access_bug.all;

architecture test of issue79 is

    function make_ptrs(init : integer) return integer_access_array is
        variable r : integer_access_array(1 to 5);
    begin
        for i in r'range loop
            r(i) := new integer'(init);
        end loop;
        return r;
    end function;

begin

    process is
        variable p : integer_access_array(1 to 5);
    begin
        assert p = (1 to 5 => null);
        p := make_ptrs(2);
        for i in p'range loop
            assert p(i).all = 2;
        end loop;
        wait;
    end process;

end architecture;

package other_pkg is
  type type_t is (value1, value2);
  function fun return integer;
  procedure proc;
end package;

package body other_pkg is
  function fun return integer is
  begin
    return 0;
  end function;

  procedure proc is
  begin
  end procedure;
end package body;

use work.other_pkg.all;

package pkg is
end package;

package body pkg is
  -- Error here when work library not called "work"
  alias pkg_t is work.other_pkg.type_t;
  procedure proc is
    variable var : integer;
  begin
    work.other_pkg.proc;
    var := work.other_pkg.fun;
  end procedure;
end package body;

package other_pkg is
  procedure other_proc;
end package;

package body other_pkg is
  procedure other_proc is
  begin
  end procedure;
end package body;

use work.other_pkg.other_proc; -- use work.other_pkg.all works though

package pkg is
end package;

package body pkg is
  procedure proc is
  begin
    other_proc;
  end procedure;
end package body;

package pkg is
  procedure proc;
end package;

package body pkg is
  procedure proc is
  begin
    report "Hello";
  end procedure;
end package body;

-------------------------------------------------------------------------------

-- use work.pkg.proc; Put use clause here and it will work

package other_pkg is
  procedure calling_proc;
end package;

use work.pkg.proc;

package body other_pkg is
  procedure calling_proc is
  begin
    proc;
  end procedure;
end package body;

-------------------------------------------------------------------------------

use work.other_pkg.calling_proc;

entity issue186 is
end entity;

architecture a of issue186 is
begin
  main : process
  begin
    calling_proc;
    wait;
  end process;
end architecture;

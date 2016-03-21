use std.textio.all;

package issue284_pkg is
   procedure check_it;
end package issue284_pkg;

package body issue284_pkg is
  file my_file : text;

  procedure check_it is
    variable contents : line;
  begin
    file_open(my_file, "test.txt", WRITE_MODE);
    write(contents, string'("hello"));
    writeline(my_file, contents);
    file_close(my_file);
  end procedure check_it;

end package body issue284_pkg;

entity issue284 is
end entity issue284;

use work.issue284_pkg.all;

architecture test of issue284 is
begin
  process
  begin
    check_it;
    wait;
  end process;
end architecture test;

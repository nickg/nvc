use std.textio.all;

package test_pkg is
  procedure report_line(
    variable reported_line : inout line
  );
end package;
package body test_pkg is
  procedure report_line(
    variable reported_line : inout line
  ) is
  begin
    report reported_line'instance_name;
  end procedure;
end package body;

use std.textio.all;
use work.test_pkg.all;

entity issue1281 is
end entity;
architecture beh of issue1281 is
begin

  process
    variable v_line : line;
  begin
    report v_line'instance_name;
    report_line(v_line);
    wait;
  end process;

end architecture beh;

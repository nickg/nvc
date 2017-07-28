entity issue335 is
end entity;

use std.textio.all;

architecture a of issue335 is
begin

  main : process is
    variable tmp : integer;
    variable l : line;
  begin
    l := new string'("1");
    report integer'image(l.all'length) & ", '" & l.all & "'";
    assert l.all = "1";

    read(l, tmp);
    assert tmp = 1;

    l := new string'("22");
    report integer'image(l.all'length) & ", '" & l.all & "'";
    assert l.all = "22";

    -- Uncomment this to make it work
    l := new string'("333");
    report integer'image(l.all'length) & ", '" & l.all & "'";
    assert l.all = "333";
    wait;
  end process;

end architecture;

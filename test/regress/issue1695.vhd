package issue1695_pkg is
  constant tag : string := "<tag>";

  procedure p generic (type t) parameter (x : t; msg : string := tag);
  function f generic (type t) parameter (x : t; msg : string := tag)
    return string;
end package;

package body issue1695_pkg is
  procedure p generic (type t) parameter (x : t; msg : string := tag) is
  begin
    assert msg = tag;
  end procedure;

  function f generic (type t) parameter (x : t; msg : string := tag)
    return string is
  begin
    return msg;
  end function;
end package body;

use work.issue1695_pkg.all;

entity issue1695 is
end entity;

architecture test of issue1695 is
  procedure p_int is new p generic map (t => integer);
  function f_int is new f generic map (t => integer);
begin
  process is
  begin
    p_int(1);
    assert f_int(1) = tag;
    report "PASSED";
    wait;
  end process;
end architecture;

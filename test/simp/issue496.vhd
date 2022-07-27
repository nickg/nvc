package test_pkg is
  type rec is record
    f : integer;
  end record;
  signal s : rec;
  function get_length(
    constant msg_id : in natural) return natural;
end package;

package body test_pkg is
function get_length(
    constant msg_id : in natural)
  return natural is
  begin
    return 1;
  end function;
end package body;

entity test is
end entity;

use work.test_pkg.all;

architecture rtl of test is
  constant one : integer := get_length(1);
begin
end architecture;

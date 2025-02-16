set -xe

nvc -a - <<-EOF
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
entity cmdline14 is
end entity;
architecture test of cmdline14 is
  signal x : integer;
  signal y : unsigned(7 downto 0) := to_unsigned(42, 8);
begin
  y <= (others => 'X');
  x <= to_integer(y);
end architecture;
EOF

nvc --ieee-warnings=on -e cmdline14 --jit --no-save -r cmdline14 2>err
grep "NUMERIC_STD.TO_INTEGER: metavalue detected, returning 0" err

nvc --ieee-warnings=off -e cmdline14 --jit --no-save -r cmdline14 2>err
! grep "NUMERIC_STD.TO_INTEGER: metavalue detected, returning 0" err

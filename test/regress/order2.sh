set -xe

pwd
which nvc

nvc -a - <<EOF
package pack is
  function foo (x : integer) return integer;
end package;

package body pack is
  function foo (x : integer) return integer is
  begin
    return x + 2;
  end function;
end package body;
EOF

nvc -a - <<EOF
package pack is
  function bar (x : real) return real;
end package;

entity order2 is
end entity;

use work.pack.all;

architecture test of order2 is
  signal r : real := bar(1.0);
begin
end architecture;
EOF

if nvc -e order2 2>err; then
  echo "should have failed!"
  exit 1
fi

grep "design unit depends on WORK.PACK with checksum" err || exit 1

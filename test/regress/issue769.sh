set -xe

pwd
which nvc

for i in 1 2; do
  nvc -a - <<EOF
package pack is
  constant k : integer := 1;
end package;

use work.all;   -- Makes "foo" visible

entity foo is
  port ( x : in integer := pack.k );
end entity;
EOF
done

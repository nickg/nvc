set -xe

pwd
which nvc

cat >pack.vhd <<EOF
package pack is
constant k : integer := 1;
end package;
EOF

mkdir -p subdir

nvc --std=1993 --work=subdir/foo.93 -a pack.vhd
nvc --std=2008 --work=subdir/foo.08 -a pack.vhd

cat >test.vhd <<EOF
library foo;
use foo.pack.all;
package blah is
constant b : integer := k;
end package;
EOF

nvc --std=1993 --map foo:subdir/foo.93 -a test.vhd
nvc --std=2008 --map foo:subdir/foo.08 -a test.vhd

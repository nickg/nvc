set -xe

pwd
which nvc

rm -rf tmpwork

nvc --work=tmpwork --std=2008 -a - <<EOF
context foo is
end context;

entity bar is
end entity;

package baz is
generic (n : integer);
end package;

package qux is new work.baz generic map (4);
EOF

nvc --work=tmpwork --list >out

diff -u - out <<EOF
TMPWORK.BAR                     : Entity
TMPWORK.BAZ                     : Package
TMPWORK.FOO                     : Context
TMPWORK.QUX                     : Instantiated package
EOF

set -xe

cat >test.v <<EOF
\`include "foo.vh"
EOF

mkdir -p inc
cat >inc/foo.vh <<EOF
module test;
initial \$display("hello, world");
endmodule
EOF

nvc -a -I inc test.v -e test -r

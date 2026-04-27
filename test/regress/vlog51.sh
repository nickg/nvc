set -xe

pwd
which nvc

cat >vlog51.v <<'EOF'
// Unresolved upward hierarchical reference: `xx` is not an instance in
// any enclosing scope.  The error must fire during elaboration, now
// that parsing defers the check for potential upward name resolution
// (IEEE 1364-2005 §12.4.2).
module vlog51_child;
    reg [3:0] r;
    initial r = xx.data;
endmodule

module vlog51;
    vlog51_child u ();
endmodule
EOF

nvc -a vlog51.v

! nvc -e vlog51 2>err

grep "no visible declaration for 'xx'" err

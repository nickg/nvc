set -xe

pwd
which nvc

# $dumpvars(0, u.u2) scope argument: dump exactly the u2 subtree.
# Verify VCD contains a scope declaration for u2 but not for its
# sibling u3 (which lives under the same parent u).

cat >vlog82.v <<'EOF'
module leaf;
    reg [3:0] val = 4'b1010;
endmodule

module mid;
    leaf l ();
endmodule

module vlog82;
    mid u ();
    mid other ();

    initial begin
        $dumpfile("vlog82.vcd");
        // Scope argument is a multi-segment hier path (IEEE 1364 §18.2.3.2).
        $dumpvars(0, u.l);
        #10;
        u.l.val = 4'b0101;
        #10;
        $finish;
    end
endmodule
EOF

nvc -a vlog82.v
nvc -e vlog82 --no-save -r

# VCD must contain a $scope for l (the dumped subtree root)
grep '\$scope.*\<l\>' vlog82.vcd

# VCD must NOT contain a $scope for other (sibling of u, outside the dumped subtree)
! grep '\$scope.*\<other\>' vlog82.vcd

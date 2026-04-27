set -xe

# Multi-top elaboration combined with a named procedural block in
# one of the tops, run under reheat (-r) so the save/restore path
# also exercises both features together.  No single existing test
# combines all three corners.
#
# The named-block local must survive serialisation and restore;
# the cross-top reference (top1 reads top2's named-block-local)
# must work after the rebuild.

pwd
which nvc

cat >vlog145_a.v <<'EOF'
module vlog145_a;
    initial begin : ablk
        reg [7:0] cell_a;
        cell_a = 8'h33;
    end
endmodule
EOF

cat >vlog145_b.v <<'EOF'
module vlog145_b;
    initial begin
        #1;
        if ($root.vlog145_a.ablk.cell_a !== 8'h33)
            $fatal(1, "FAIL: $root.a.ablk.cell_a=%h",
                       $root.vlog145_a.ablk.cell_a);
        if (vlog145_a.ablk.cell_a !== 8'h33)
            $fatal(1, "FAIL bare: a.ablk.cell_a=%h",
                       vlog145_a.ablk.cell_a);
        $display("PASSED");
    end
endmodule
EOF

nvc -a vlog145_a.v vlog145_b.v \
    -e vlog145_b vlog145_a \
    -r --stop-time=5ns >out 2>&1

grep "PASSED" out

set -xe

# Multi-instance module containing a V_HIER_REF (no named procedural
# blocks) — pins the per-clone gating path for the pure
# V_HIER_REF-only case.  Each clone's resolved hier-ref target is
# the same across instances (a sibling top), but the gating must
# still treat the module as having per-clone state on the IR node
# directly (the resolver writes I_REF / I_IDENT2).
#
# Runs as a shell test because the design uses multi-top elab
# (synthetic root + sibling reach via $root).

pwd
which nvc

cat >vlog144_shared.v <<'EOF'
module vlog144_shared;
    reg [7:0] count;
    initial count = 8'h00;
endmodule
EOF

cat >vlog144_top.v <<'EOF'
module vlog144_reader(input [3:0] tag);
    wire [7:0] saw;
    assign saw = $root.vlog144_shared.count;
    wire [3:0] tagx = tag ^ 4'hF;
endmodule

module vlog144_top;
    vlog144_reader u0(.tag(4'h1));
    vlog144_reader u1(.tag(4'h2));
    vlog144_reader u2(.tag(4'h3));
    initial begin
        #1;
        if (u0.saw !== 8'h00) $fatal(1, "FAIL u0.saw=%h", u0.saw);
        if (u1.saw !== 8'h00) $fatal(1, "FAIL u1.saw=%h", u1.saw);
        if (u2.saw !== 8'h00) $fatal(1, "FAIL u2.saw=%h", u2.saw);
        if (u0.tagx !== 4'hE) $fatal(1, "FAIL u0.tagx=%h", u0.tagx);
        if (u1.tagx !== 4'hD) $fatal(1, "FAIL u1.tagx=%h", u1.tagx);
        if (u2.tagx !== 4'hC) $fatal(1, "FAIL u2.tagx=%h", u2.tagx);
        $display("PASSED");
    end
endmodule
EOF

nvc -a vlog144_shared.v vlog144_top.v \
    -e vlog144_top vlog144_shared \
    -r --stop-time=5ns >out 2>&1

grep "PASSED" out

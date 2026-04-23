set -xe

pwd
which nvc

# Reheat / save-restore test for hierarchical references.
#
# Shape: multi-instance clones with hier-ref reads and writes (like vlog49).
# Strategy: elaborate WITHOUT --no-save (so the design is persisted to disk),
# run once, then run again from the saved elaboration (reheat path).  The
# second run must produce the same correct result, proving that hier-ref
# resolver state survives serialisation and reheat.
#
# The nvc test harness triggers reheat by splitting elaborate and run into
# two separate nvc invocations:
#   nvc -a ... -e TOP         (saves elaborated design)
#   nvc -r TOP                (reheats from saved state, then simulates)
# Running -r a second time exercises the reheat code path again.

cat >vlog84.v <<'EOF'
module leaf;
    reg [7:0] q = 8'd0;
endmodule

module group;
    leaf a ();
    leaf b ();
endmodule

module vlog84;
    group g1 ();
    group g2 ();

    initial begin
        #1;
        // Write through hier refs into cloned instances
        g1.a.q = 8'd11;
        g1.b.q = 8'd22;
        g2.a.q = 8'd33;
        g2.b.q = 8'd44;
        #1;
        // Read back through hier refs — each clone must keep its own value
        if (g1.a.q !== 8'd11 || g1.b.q !== 8'd22 ||
            g2.a.q !== 8'd33 || g2.b.q !== 8'd44) begin
            $display("FAILED: g1.a.q=%0d g1.b.q=%0d g2.a.q=%0d g2.b.q=%0d",
                     g1.a.q, g1.b.q, g2.a.q, g2.b.q);
            $finish;
        end
        $display("PASSED");
        $finish;
    end
endmodule
EOF

# First pass: analyse and elaborate (saves to disk)
nvc -a vlog84.v
nvc -e vlog84

# First run from saved elaboration (reheat path)
nvc -r vlog84 >stdout1 2>stderr1
grep PASSED stdout1

# Second run from the same saved elaboration (reheat again)
nvc -r vlog84 >stdout2 2>stderr2
grep PASSED stdout2

set -xe

# Regression: cross-top hierarchical references under multi-top elab.
# Two complementary forms exercise the synthetic root:
#
#   - $root.<sibling>.<sig>  — absolute, anchored at the synthetic
#     root above all user tops.
#
#   - <sibling>.<sig>        — bare-name; relative walk-up reaches
#     the synthetic-root sentinel and synthesises the shared library
#     prefix from a sibling top's dotted.
#
# Both directions are exercised: each top reads a signal from its
# sibling top.  Symmetric coverage catches one-direction anchoring
# bugs.

pwd
which nvc

cat >vlog138_glbl.v <<'EOF'
module vlog138_glbl;
    reg gsr;
    reg saw_dut_alive;
    initial begin
        gsr = 1'b1;
        saw_dut_alive = 1'b0;
        #2;
        // Bare-name reverse direction: glbl reads DUT's `alive`.
        if (vlog138_dut.alive !== 1'b1)
            $fatal(1, "FAIL: glbl saw alive=%b",
                       vlog138_dut.alive);
        // $root form same direction.
        if ($root.vlog138_dut.alive !== 1'b1)
            $fatal(1, "FAIL: glbl saw $root.dut.alive=%b",
                       $root.vlog138_dut.alive);
        saw_dut_alive = 1'b1;
    end
endmodule
EOF

cat >vlog138_dut.v <<'EOF'
module vlog138_dut;
    reg alive;
    initial begin
        alive = 1'b1;
        #1;
        // $root.<sibling>.<sig>: absolute anchor.
        if ($root.vlog138_glbl.gsr !== 1'b1)
            $fatal(1, "FAIL: $root.glbl.gsr=%b",
                       $root.vlog138_glbl.gsr);
        // <sibling>.<sig>: bare-name, library-prefix synthesis.
        if (vlog138_glbl.gsr !== 1'b1)
            $fatal(1, "FAIL: glbl.gsr=%b", vlog138_glbl.gsr);
        #2;
        // Confirm glbl finished its checks against us.
        if (vlog138_glbl.saw_dut_alive !== 1'b1)
            $fatal(1, "FAIL: glbl did not see dut.alive");
        $display("PASSED");
    end
endmodule
EOF

nvc -a vlog138_glbl.v vlog138_dut.v \
    -e vlog138_dut vlog138_glbl \
    -r --stop-time=10ns >out 2>&1

grep "PASSED" out

# Three-top variant — verifies the bare-name walk-up reaches more
# than one sibling under the synthetic root and the library-prefix
# synthesis works regardless of which sibling we land on.

rm -rf work    # clean work library between 2-top and 3-top phases

cat >vlog138_a.v <<'EOF'
module vlog138_a;
    reg [7:0] tag_a;
    initial tag_a = 8'hAA;
endmodule
EOF

cat >vlog138_b.v <<'EOF'
module vlog138_b;
    reg [7:0] tag_b;
    initial tag_b = 8'h55;
endmodule
EOF

cat >vlog138_dut3.v <<'EOF'
module vlog138_dut3;
    initial begin
        #1;
        if (vlog138_a.tag_a !== 8'hAA)
            $fatal(1, "FAIL three-top: a.tag_a=%h", vlog138_a.tag_a);
        if (vlog138_b.tag_b !== 8'h55)
            $fatal(1, "FAIL three-top: b.tag_b=%h", vlog138_b.tag_b);
        if ($root.vlog138_a.tag_a !== 8'hAA)
            $fatal(1, "FAIL three-top: $root.a.tag_a=%h",
                       $root.vlog138_a.tag_a);
        if ($root.vlog138_b.tag_b !== 8'h55)
            $fatal(1, "FAIL three-top: $root.b.tag_b=%h",
                       $root.vlog138_b.tag_b);
        $display("PASSED");
    end
endmodule
EOF

nvc -a vlog138_a.v vlog138_b.v vlog138_dut3.v \
    -e vlog138_dut3 vlog138_a vlog138_b \
    -r --stop-time=5ns >out3 2>&1

grep "PASSED" out3

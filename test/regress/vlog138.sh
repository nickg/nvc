set -xe

# Regression: $root anchored cross-top absolute paths.  A DUT top
# references a signal in a sibling glbl top via $root.glbl.gsr —
# the Xilinx UNISIM GSR idiom.  Previously $root anchored at the
# topmost Verilog ancestor of the caller (i.e., the DUT's own
# subtree), so the sibling was unreachable.

pwd
which nvc

cat >vlog138_glbl.v <<'EOF'
module vlog138_glbl;
    reg gsr;
    initial gsr = 1'b1;
endmodule
EOF

cat >vlog138_dut.v <<'EOF'
module vlog138_dut;
    initial begin
        #1;
        if ($root.vlog138_glbl.gsr !== 1'b1)
            $fatal(1, "FAIL: gsr=%b", $root.vlog138_glbl.gsr);
        $display("PASSED");
    end
endmodule
EOF

nvc -a vlog138_glbl.v vlog138_dut.v \
    -e vlog138_dut vlog138_glbl \
    -r --stop-time=5ns >out 2>&1

grep "PASSED" out

// Test SystemVerilog $root absolute path (IEEE 1800-2017 S23.3.1).
//
// $root.vlog63.x is an absolute path anchored at the elaboration
// root, accessed from inside a nested instance.  This is distinct
// from the IEEE 1364 rooted path (vlog62) in that it uses the
// explicit $root keyword.

module vlog63_inner;
    reg [7:0] captured;
    initial begin
        #1;
        captured = $root.vlog63.x;
    end
endmodule

module vlog63_mid;
    vlog63_inner leaf ();
endmodule

module vlog63;
    reg [7:0] x = 8'h5A;

    vlog63_mid u ();

    initial begin
        #2;
        if (u.leaf.captured !== 8'h5A) begin
            $display("FAILED: u.leaf.captured=%h expected 5A",
                     u.leaf.captured);
            $finish;
        end

        // Verify $root from the top level itself works too
        if ($root.vlog63.x !== 8'h5A) begin
            $display("FAILED: $root.vlog63.x=%h expected 5A",
                     $root.vlog63.x);
            $finish;
        end

        $display("PASSED");
        $finish;
    end
endmodule

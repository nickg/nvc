// Test downward hierarchical name resolution (IEEE 1364-2005 §12.4).
// `u1.x` refers to a sub-instance of vlog52.  The reference appears
// textually before `u1` is declared, exercising the deferred-resolution
// path so the resolver must cope with the target instance not being in
// the module cache until after its sibling sub-blocks have been
// elaborated.

module vlog52_sub;
    reg [3:0] x = 4'h7;
endmodule

module vlog52;
    reg [3:0] r;
    initial begin
        #1;
        r = u1.x;
        if (r !== 4'h7) begin
            $display("FAILED: r=%h expected 7", r);
            $finish;
        end
        $display("PASSED");
        $finish;
    end

    vlog52_sub u1 ();
endmodule

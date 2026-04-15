// Test multi-level upward name resolution and mixed upward/downward
// hierarchical references (IEEE 1364-2005 S12.4.2).
//
// 3-deep hierarchy: vlog59 -> mid -> inner.
// From inner:
//   1. "peer.data" resolves upward to vlog59's scope, finds sibling
//      instance "peer" (a sibling of the ancestor "mid"), then
//      descends into peer.data.
//   2. "mid_sib.leaf.val" resolves upward past mid to vlog59,
//      finds sibling instance "mid_sib", then descends through
//      mid_sib.leaf.val.

module vlog59_leaf;
    reg [7:0] val = 8'hBB;
endmodule

module vlog59_peer;
    reg [7:0] data = 8'hAA;
endmodule

module vlog59_inner;
    reg [7:0] r1;
    reg [7:0] r2;
    initial begin
        #1;
        // Upward to ancestor (vlog59), then down into sibling "peer"
        r1 = peer.data;
        // Upward to ancestor (vlog59), then down into "mid_sib.leaf.val"
        r2 = mid_sib.leaf.val;
    end
endmodule

module vlog59_mid;
    vlog59_inner inner ();
endmodule

module vlog59_mid_sib;
    vlog59_leaf leaf ();
endmodule

module vlog59;
    vlog59_peer    peer    ();
    vlog59_mid     mid     ();
    vlog59_mid_sib mid_sib ();

    initial begin
        #2;
        if (mid.inner.r1 !== 8'hAA) begin
            $display("FAILED: mid.inner.r1=%h expected AA", mid.inner.r1);
            $finish;
        end
        if (mid.inner.r2 !== 8'hBB) begin
            $display("FAILED: mid.inner.r2=%h expected BB", mid.inner.r2);
            $finish;
        end
        $display("PASSED");
        $finish;
    end
endmodule

// Test named statement block and fork/join label prefixes in
// hierarchical references (IEEE 1364-2005 S12.4.3, S9.8).
//
// A named begin block ("begin : blk1") and a fork/join block
// ("fork : fk") each introduce a scope with locally-declared
// variables.  These variables are accessible via hierarchical
// path from outside the block: vlog60.u.blk1.x and vlog60.u.fk.y.

module vlog60_sub;
    initial begin : blk1
        reg [7:0] x;
        x = 8'h42;
    end

    initial fork : fk
        reg [7:0] y;
        y = 8'hDE;
    join
endmodule

module vlog60;
    vlog60_sub u ();

    initial begin
        #1;
        if (u.blk1.x !== 8'h42) begin
            $display("FAILED: u.blk1.x=%h expected 42", u.blk1.x);
            $finish;
        end
        if (u.fk.y !== 8'hDE) begin
            $display("FAILED: u.fk.y=%h expected DE", u.fk.y);
            $finish;
        end
        $display("PASSED");
        $finish;
    end
endmodule

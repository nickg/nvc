// Test procedural assignment through a hierarchical reference and
// hierarchical reference in always sensitivity list.

module sub;
    reg [3:0] q = 0;
endmodule

module vlog48;
    sub u ();
    reg [3:0] r = 0;

    always @(u.q) r = u.q;

    initial begin
        #1 u.q = 4'd9;
        #1;
        if (r !== 4'd9) begin
            $display("FAILED: r=%0d expected 9", r);
            $finish;
        end
        $display("PASSED");
        $finish;
    end
endmodule

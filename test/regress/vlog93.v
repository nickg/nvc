// Event expression variants with hierarchical references.
// Fills a gap left by vlog48 which covers only level-change @(u.q).
//   @(posedge u.clk) — edge expression on hier-addressed clock
//   @(u.a or u.b)    — combined event expression with multiple hier refs

module vlog93_src;
    reg clk = 0;
    reg a = 0;
    reg b = 0;
    always #5 clk = ~clk;
endmodule

module vlog93;
    vlog93_src u ();
    reg [3:0] posedge_count = 0;
    reg [3:0] or_count = 0;

    // Edge expression on a hier-addressed clock.
    always @(posedge u.clk) posedge_count <= posedge_count + 1;

    // Combined event expression: two hier refs joined with `or`.
    always @(u.a or u.b) or_count <= or_count + 1;

    initial begin
        // Wait past 2 posedges (at t=5 and t=15).
        #22;
        if (posedge_count < 2) begin
            $display("FAILED: posedge_count=%0d expected >=2", posedge_count);
            $finish;
        end

        // Drive each `or` operand; expect or_count to bump twice.
        u.a = 1'b1;
        #1;
        u.b = 1'b1;
        #1;
        if (or_count < 2) begin
            $display("FAILED: or_count=%0d expected >=2", or_count);
            $finish;
        end

        $display("PASSED");
        $finish;
    end
endmodule

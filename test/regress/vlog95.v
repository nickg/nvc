// RHS concatenation of hier refs, direct (no macro indirection).
// Fills a gap: vlog46 covers {u.b, u.a} as LHS concat, vlog49 covers
// RHS concat but only via macro expansion.  This test exercises the
// direct form where the parser sees the literal concat operator with
// hier-ref operands.

module vlog95_src;
    reg [3:0] a = 4'hA;
    reg [3:0] b = 4'hB;
endmodule

module vlog95;
    vlog95_src u ();
    reg [7:0] combined;

    initial begin
        #1;
        combined = {u.a, u.b};
        if (combined !== 8'hAB) begin
            $display("FAILED: {u.a, u.b}=%h expected AB", combined);
            $finish;
        end
        $display("PASSED");
        $finish;
    end
endmodule

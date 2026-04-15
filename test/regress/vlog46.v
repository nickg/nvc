// Test concatenation of hierarchical references as LHS in procedural block.

module vlog46_inner;
    reg a = 0;
    reg b = 0;
endmodule

module vlog46;
    vlog46_inner u ();

    initial begin
        {u.b, u.a} <= 2'b10;
        #1;
        if ({u.b, u.a} !== 2'b10) begin
            $display("FAILED: {u.b,u.a}=%b expected 10", {u.b, u.a});
            $finish;
        end
        $display("PASSED");
        $finish;
    end
endmodule

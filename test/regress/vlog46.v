// Test `define with trailing // comment (IEEE 1364-2005 §19.3.1).
// The comment is NOT part of the macro text.

`define VAL1 10'b0011110110  // trailing comment
`define VAL2 42              // another comment
`define VAL3 "hello"         // string value

module vlog46;
    initial begin
        if (`VAL1 !== 10'b0011110110) begin
            $display("FAILED: VAL1=%b", `VAL1);
            $finish;
        end
        if (`VAL2 !== 42) begin
            $display("FAILED: VAL2=%0d", `VAL2);
            $finish;
        end
        $display("PASSED");
        $finish;
    end
endmodule

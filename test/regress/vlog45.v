// Test string comparison in parameter ternary expression.
// Exercises constant folding of string equality for single-word
// and multi-word strings (>8 chars = >64 bits).

module vlog45 #(
    parameter STYLE = "abc"
);

    // Single-word comparison: "abc" == "abc" (24-bit, 1 word)
    parameter P1 = (STYLE == "abc") ? "longstring1" : STYLE;

    // Multi-word equal: "longstring1" == "longstring1" (88-bit, 2 words)
    parameter MW_EQ = (P1 == "longstring1") ? 1 : 0;

    // Multi-word not-equal: "longstring1" vs "longstring2" (88-bit, 2 words)
    parameter MW_NEQ = ("longstring1" == "longstring2") ? 0 : 1;

    // Different lengths: "abc" (24-bit) vs "longstring1" (88-bit)
    parameter DIFF_LEN = ("abc" == "longstring1") ? 0 : 1;

    // Case sensitivity: "abc" != "ABC"
    parameter CASE_SENS = ("abc" == "ABC") ? 0 : 1;

    generate
        if (MW_EQ && MW_NEQ && DIFF_LEN && CASE_SENS) begin
            initial begin
                $display("PASSED");
                $finish;
            end
        end
        else begin
            initial begin
                $display("FAILED: MW_EQ=%0d MW_NEQ=%0d DIFF_LEN=%0d CASE_SENS=%0d",
                         MW_EQ, MW_NEQ, DIFF_LEN, CASE_SENS);
                $finish;
            end
        end
    endgenerate

endmodule

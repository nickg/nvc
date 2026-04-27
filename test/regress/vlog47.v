// Test Verilog hierarchical references (IEEE 1364-2005 S12.4).
// Access signals inside submodule instances via dot notation,
// including nested hierarchy.

module vlog47_leaf(input wire clk);
    reg [7:0] count = 0;
    always @(posedge clk)
        count <= count + 1;
endmodule

module vlog47_mid(input wire clk);
    reg [3:0] mid_val = 4'd9;
    vlog47_leaf leaf (.clk(clk));
endmodule

module vlog47;
    reg clk = 0;
    always #5 clk = ~clk;

    vlog47_mid u (.clk(clk));

    initial begin
        #25;
        // One level deep
        if (u.mid_val !== 4'd9) begin
            $display("FAILED: u.mid_val=%0d expected 9", u.mid_val);
            $finish;
        end
        // Two levels deep
        if (u.leaf.count !== 8'd2) begin
            $display("FAILED: u.leaf.count=%0d expected 2", u.leaf.count);
            $finish;
        end
        $display("PASSED");
        $finish;
    end
endmodule

// Test wire with initialiser inside generate block.

module vlog38;
    wire [3:0] result;

    generate if (1) begin: gen_main
        wire [3:0] local_val = 4'd5;
        assign result = local_val;
    end
    endgenerate

    initial begin
        #10;
        if (result !== 4'd5) begin
            $display("FAILED: expected 5, got %0d", result);
            $finish;
        end

        $display("PASSED");
        $finish;
    end
endmodule

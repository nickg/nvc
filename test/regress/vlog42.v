// Test bare $fatal (IEEE 1800-2017 §20.11).

module vlog42;
    initial begin
        $display("PASSED");
        $fatal;
    end
endmodule

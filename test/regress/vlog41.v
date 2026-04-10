// Test $fatal(1, fmt, args) (IEEE 1800-2017 §20.11).

module vlog41;
    initial begin
        $display("PASSED");
        $fatal(1, "fatal1: x=%0d", 99);
    end
endmodule

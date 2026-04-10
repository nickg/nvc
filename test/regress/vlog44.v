// Test $fatal(2, fmt, multiple args) (IEEE 1800-2017 §20.11).

module vlog44;
    initial begin
        $display("PASSED");
        $fatal(2, "fatal2: a=%0d b=%s", 42, "hello");
    end
endmodule

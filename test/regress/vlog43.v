// Test $fatal(0) — finish_number only (IEEE 1800-2017 §20.11).

module vlog43;
    initial begin
        $display("PASSED");
        $fatal(0);
    end
endmodule

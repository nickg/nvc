// Test program block prefix in hierarchical references
// (IEEE 1800-2017 S24).
//
// A program block defines an addressable scope like a module.
// When instantiated, its internal signals are accessible via
// hierarchical path through the instance name.

program vlog66_prog;
    reg [7:0] x = 8'h3C;
    reg [7:0] y = 8'hA5;
endprogram

module vlog66;
    vlog66_prog prog_inst ();

    initial begin
        #1;
        if (prog_inst.x !== 8'h3C) begin
            $display("FAILED: prog_inst.x=%h expected 3C", prog_inst.x);
            $finish;
        end
        if (prog_inst.y !== 8'hA5) begin
            $display("FAILED: prog_inst.y=%h expected A5", prog_inst.y);
            $finish;
        end

        // Write through the hierarchical path and verify
        prog_inst.x = 8'hFF;
        #1;
        if (prog_inst.x !== 8'hFF) begin
            $display("FAILED: prog_inst.x=%h expected FF after write",
                     prog_inst.x);
            $finish;
        end

        $display("PASSED");
        $finish;
    end
endmodule

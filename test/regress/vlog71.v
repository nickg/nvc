// defparam target via hierarchical reference (IEEE 1364 S12.2.1).
//
// Submodule has parameter WIDTH = 4 and a reg sized by it.
// Top overrides: defparam u.WIDTH = 8;
// Verify the reg is 8 bits wide by writing a value that needs all 8 bits.

module vlog71_sub;
    parameter WIDTH = 4;
    reg [WIDTH-1:0] data;

    initial data = 0;
endmodule

module vlog71;
    defparam u.WIDTH = 8;

    vlog71_sub u ();

    initial begin
        #1;
        u.data = 8'hFF;
        #1;

        // If WIDTH were still 4, only the low 4 bits would survive.
        if (u.data !== 8'hFF) begin
            $display("FAILED: u.data=%h expected FF (WIDTH override failed)",
                     u.data);
            $finish;
        end

        // Double-check: a value with upper bits set.
        u.data = 8'hA5;
        #1;
        if (u.data !== 8'hA5) begin
            $display("FAILED: u.data=%h expected A5", u.data);
            $finish;
        end

        $display("PASSED");
        $finish;
    end
endmodule

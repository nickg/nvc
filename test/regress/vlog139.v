// Cross-module hierarchical reference reaching a Verilog signal
// through a VHDL component-binding boundary.  The VHDL top
// component-binds two Verilog modules; a continuous assign in `mid`
// references `U_GLBL.glbl.marker`, walking up out of `mid` through
// the VHDL parent, across to the sibling VHDL component `U_GLBL`,
// then down into its bound Verilog `glbl` to read `marker`.

module glbl;
    wire [7:0] marker = 8'hA5;
endmodule

module mid;
    wire [7:0] got;
    assign got = U_GLBL.glbl.marker;

    initial begin
        #1;
        if (got !== 8'hA5) begin
            $display("FAIL: got=%h expected A5", got);
            $finish;
        end
        // Direct XMR (not through assign) too
        if (U_GLBL.glbl.marker !== 8'hA5) begin
            $display("FAIL: direct XMR got %h", U_GLBL.glbl.marker);
            $finish;
        end
    end
endmodule

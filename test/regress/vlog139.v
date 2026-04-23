// Regression for cross-module hierarchical reference reaching a
// Verilog signal through a VHDL component-binding boundary.
//
// Architecture: VHDL top component-binds two Verilog modules.  A
// continuous assign in `mid` references `U_GLBL.glbl.marker` —
// walking up out of `mid` through the VHDL parent, across to the
// sibling VHDL component `U_GLBL`, then down into its bound
// Verilog `glbl` to read `marker`.
//
// Pre-fix bug: the resolver's I_IDENT2 (used by link_package at
// runtime) and vlog_lower_block's instance_init key (used by
// PUTPRIV when initialising the scope) used different names — the
// resolver used `cloned + chain`, instance_init used the per-clone
// alias.  In the VHDL-wrapped case neither got the per-instance
// dotted path right, so PUTPRIV/GETPRIV ran under different JIT
// handles and link_var faulted on a NULL privdata.
//
// Fix: vlog_lower_block registers the wrapper under
// `<dotted>$instance` so the bare dotted path can alias the
// shared template; resolver emits the dotted path on I_IDENT2;
// instance_init uses the dotted path as its PUTPRIV key.  All
// three sites name the scope identically.

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

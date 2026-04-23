// Regression for the Verilog hier-ref resolver's single-source-of-
// truth invariant.  The resolver (elab_resolve_all_vlog_hier_refs),
// registration (vlog_lower_block) and the elab-time alias
// computations all go through vlog_scope_alias() — so the ident
// stored on each V_HIER_REF's I_IDENT2 matches the one that
// mir_alias_unit registers.
//
// This test drives a child's internal wires through forward cross-
// module reference continuous assigns (`assign I_leaf.foo = …`)
// from a Verilog subtree that is itself reached through a VHDL
// top.  Before the fix, `elab_resolve_all_vlog_hier_refs` was only
// called from the Verilog-root entry, so under a VHDL top the XMR
// was left unresolved and a stale fallback in `vlog_hier_unit_alias`
// produced a short module-qualified name that did not match the
// per-instance alias — link_package missed and the body was
// reported missing.

module leaf;
    wire [7:0] foo;
    wire [7:0] bar;
endmodule

module mid;
    assign I_leaf.foo = 8'hAA;
    assign I_leaf.bar = 8'h55;

    leaf I_leaf();

    initial begin
        #1;
        if (I_leaf.foo !== 8'hAA) begin
            $display("FAIL: foo=%h", I_leaf.foo);
            $finish;
        end
        if (I_leaf.bar !== 8'h55) begin
            $display("FAIL: bar=%h", I_leaf.bar);
            $finish;
        end
    end
endmodule

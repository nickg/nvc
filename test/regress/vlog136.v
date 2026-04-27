// Hierarchical references through a Verilog subtree reached under
// a VHDL top.  Two complementary forms exercised:
//
//   1. Module-instance XMR — forward continuous-assign +
//      read-from-outside.  A child's internal wires are driven via
//      `assign I_leaf.foo = …` from a Verilog subtree reached
//      through a VHDL top.  Verifies the resolver runs under any
//      root and the alias produced by lowering matches the
//      per-instance link_package key registered at create_scope
//      time.
//
//   2. Named procedural block scope (IEEE 1800-2017 §23.6).  An
//      `initial begin : init_blk` block declares its own reg and
//      writes to it; a sibling initial reads `init_blk.my_reg`
//      and `init_blk.inner.deep_reg` (nested named blocks).
//      Verifies that the named V_BLOCK is enumerable by the
//      resolver, that block-local decls are allocated in the
//      wrapper unit whose link_package alias matches the one
//      written on V_HIER_REF, and that writes from within the
//      block target that same wrapper-unit storage so external
//      reads observe the procedural value.
//
// Wire continuous-assigns also read from named-block locals to
// pin the path that lowers a V_HIER_REF inside a wire-init
// expression rather than a procedural rvalue.

module leaf;
    wire [7:0] foo;
    wire [7:0] bar;
endmodule

module mid;
    assign I_leaf.foo = 8'hAA;
    assign I_leaf.bar = 8'h55;

    wire [7:0] mirror_my   = init_blk.my_reg;
    wire [7:0] mirror_deep = init_blk.inner.deep_reg;

    leaf I_leaf();

    initial begin : init_blk
        reg [7:0] my_reg;
        my_reg = 8'h99;
        begin : inner
            reg [7:0] deep_reg;
            deep_reg = 8'h42;
        end
        // Read-after-write of a block local across a wait inside
        // the same procedural body.  Exercises the named-block
        // frame redirect across an MIR basic-block split: any SSA
        // value cached at frame-push time would be dead in the
        // post-wait block.
        #1;
        my_reg = my_reg + 1;
    end

    initial begin
        #2;
        if (I_leaf.foo !== 8'hAA) begin
            $display("FAIL: foo=%h", I_leaf.foo);
            $finish;
        end
        if (I_leaf.bar !== 8'h55) begin
            $display("FAIL: bar=%h", I_leaf.bar);
            $finish;
        end
        if (init_blk.my_reg !== 8'h9A) begin
            $display("FAIL: init_blk.my_reg=%h", init_blk.my_reg);
            $finish;
        end
        if (init_blk.inner.deep_reg !== 8'h42) begin
            $display("FAIL: init_blk.inner.deep_reg=%h",
                     init_blk.inner.deep_reg);
            $finish;
        end
        if (mirror_my !== 8'h9A) begin
            $display("FAIL: mirror_my=%h", mirror_my);
            $finish;
        end
        if (mirror_deep !== 8'h42) begin
            $display("FAIL: mirror_deep=%h", mirror_deep);
            $finish;
        end
    end
endmodule

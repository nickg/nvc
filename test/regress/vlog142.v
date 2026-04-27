// Named procedural block nested inside a for-generate iteration —
// covers the interaction between named-block elab+lower and
// generate-block iteration deep-copy.  Two regressions land here
// together:
//
//   1. copy_generate_pred must deep-copy named V_BLOCKs (and
//      V_HIER_REFs) per iteration.  Sharing them across iterations
//      causes elab_verilog_block's I_IDENT2 stamp to overwrite
//      itself — only the last iteration's dotted would survive.
//
//   2. elab_verilog_block must descend into `body` (the V_INST_
//      BODY copy used by vlog_lower_instance), not `v` (the
//      original V_BLOCK from the parent's stmts).  The two are
//      separate object trees after vlog_new_instance; stamping
//      ident2 on `v`'s tree leaves `body`'s named V_BLOCKs
//      unstamped, and the named-block frame is never pushed at
//      lower time.  The named-block local writes silently miss
//      the wrapper unit's signal storage and external hier-ref
//      reads see X.

module vlog142;
   genvar i;
   generate
      for (i = 0; i < 3; i = i + 1) begin : g
         initial begin : blk
            reg [7:0] r;
            r = i + 8'h10;     // per-iteration value
         end
      end
   endgenerate

   initial begin
      #1;
      if (g[0].blk.r !== 8'h10)
        $fatal(1, "FAIL g[0].blk.r=%h", g[0].blk.r);
      if (g[1].blk.r !== 8'h11)
        $fatal(1, "FAIL g[1].blk.r=%h", g[1].blk.r);
      if (g[2].blk.r !== 8'h12)
        $fatal(1, "FAIL g[2].blk.r=%h", g[2].blk.r);
      $display("PASSED");
   end
endmodule

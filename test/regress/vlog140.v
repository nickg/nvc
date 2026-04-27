// Multi-instance module containing a named procedural block.
// Each clone must end up with its own MIR (per-clone gating in
// elab_module_needs_per_clone_state should refuse cache-hit
// sharing because the body has a named V_BLOCK).  Per-clone names
// of the wrapper unit must differ so each instance's link_package
// finds its own storage.
//
// Also covers force/release into a named-block local — exercises
// the named-block frame redirect path for V_FORCE / V_RELEASE
// targets.  IEEE 1800 §10.6.2 allows force on any variable; this
// pins the force/release regression for procedural-block locals
// alongside the existing module-level reg coverage.

module vlog140_child(input [3:0] tag);
   wire [7:0] mirror;
   assign mirror = init_blk.cell;
   initial begin : init_blk
      reg [7:0] cell;
      cell = {tag, tag};   // unique per instance
   end
endmodule

module vlog140;
   vlog140_child u0(.tag(4'h1));
   vlog140_child u1(.tag(4'h2));
   vlog140_child u2(.tag(4'h3));

   initial begin
      #1;
      // Each clone has its own storage — values are tag-derived.
      if (u0.init_blk.cell !== 8'h11)
        $fatal(1, "FAIL u0: %h", u0.init_blk.cell);
      if (u1.init_blk.cell !== 8'h22)
        $fatal(1, "FAIL u1: %h", u1.init_blk.cell);
      if (u2.init_blk.cell !== 8'h33)
        $fatal(1, "FAIL u2: %h", u2.init_blk.cell);

      // Wire mirror reads the named-block local — same storage.
      if (u0.mirror !== 8'h11) $fatal(1, "FAIL u0.mirror: %h", u0.mirror);
      if (u1.mirror !== 8'h22) $fatal(1, "FAIL u1.mirror: %h", u1.mirror);
      if (u2.mirror !== 8'h33) $fatal(1, "FAIL u2.mirror: %h", u2.mirror);

      // Force one clone's local — others must remain untouched.
      force u1.init_blk.cell = 8'hAA;
      #1;
      if (u1.init_blk.cell !== 8'hAA)
        $fatal(1, "FAIL force: %h", u1.init_blk.cell);
      if (u0.init_blk.cell !== 8'h11)
        $fatal(1, "FAIL force isolation u0: %h", u0.init_blk.cell);
      if (u2.init_blk.cell !== 8'h33)
        $fatal(1, "FAIL force isolation u2: %h", u2.init_blk.cell);
      if (u1.mirror !== 8'hAA)
        $fatal(1, "FAIL force mirror: %h", u1.mirror);

      release u1.init_blk.cell;
      #1;
      // After release, value is retained until next driver write.
      // No driver in the named block writes again, so it stays.
      if (u1.init_blk.cell !== 8'hAA)
        $fatal(1, "FAIL release retain: %h", u1.init_blk.cell);

      $display("PASSED");
   end
endmodule

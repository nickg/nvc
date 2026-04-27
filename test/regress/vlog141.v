// Cross-language hier-ref reaching a Verilog module's named-block
// local through VHDL component binding.  Two instances of the same
// Verilog module (each containing a named V_BLOCK) elaborate
// correctly under VHDL — per-clone gating refuses cache-hit
// sharing for hier-ref / named-block bodies, so each instance gets
// its own MIR and its own per-instance link_package alias.
//
// The Verilog module's `assign` reaches into its own named-block
// local; if clone-sharing kicked in incorrectly, both instances
// would observe the same first-clone storage and the second
// instance's `tag` port would not affect mirror.

module vlog141_mid(output [7:0] mirror, input [3:0] tag);
   assign mirror = blk.local_val;
   initial begin : blk
      reg [7:0] local_val;
      local_val = {tag, tag};
   end
endmodule

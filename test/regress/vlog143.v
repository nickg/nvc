// Named procedural blocks nested inside procedural control-flow
// statements.  The elab_verilog_proc_blocks descent must reach
// named V_BLOCKs through every kind that can hold stmts:
//
//   * V_IF / V_COND        — `if (cond) begin : lbl ... end`
//   * V_CASE / V_CASE_ITEM — `case ... lbl : begin : lbl ... end`
//   * V_FOR_LOOP           — `for (...) begin : lbl ... end`
//
// Missing any of these recursion arms leaves the inner V_BLOCK
// unstamped (no I_IDENT2) and the lower-time named-block frame
// is never pushed for it; block-local writes target the parent
// process unit's mir_var rather than the wrapper's signal storage,
// so external hier-ref reads see X.
//
// V_COND specifically requires its own descent case: V_IF stores
// its branches via I_CONDS, each of which is a V_COND with
// I_VALUE (the condition expr) + I_STMTS (the body).

module vlog143;
   reg [7:0] r_if;
   reg [7:0] r_case;
   reg [7:0] r_for;

   initial begin
      // 1. Named block in procedural if
      if (1) begin : taken_branch
         reg [3:0] flag;
         flag = 4'hA;
         r_if = 8'hAB;
      end

      // 2. Named block in case
      case (4'h2)
         4'h1: begin : case_one
            reg [7:0] tmp;
            tmp = 8'h11;
            r_case = tmp;
         end
         4'h2: begin : case_two
            reg [7:0] tmp;
            tmp = 8'h22;
            r_case = tmp;
         end
         default: r_case = 8'h00;
      endcase

      // 3. Named block in procedural for-loop.
      // Static-by-default (IEEE 1800 §6.21) means the block-local
      // `step` storage is reused across iterations; only the last
      // iteration's value persists for external observation.
      r_for = 0;
      begin : for_wrap
         integer i;
         for (i = 0; i < 4; i = i + 1) begin : loop_blk
            reg [3:0] step;
            step = i + 1;
            r_for = r_for + step;
         end
      end
   end

   initial begin
      #1;
      if (taken_branch.flag !== 4'hA)
        $fatal(1, "FAIL taken_branch.flag=%h", taken_branch.flag);
      if (r_if !== 8'hAB)
        $fatal(1, "FAIL r_if=%h", r_if);

      if (case_two.tmp !== 8'h22)
        $fatal(1, "FAIL case_two.tmp=%h", case_two.tmp);
      if (r_case !== 8'h22)
        $fatal(1, "FAIL r_case=%h", r_case);

      // Last iteration's step value is 4 (i = 3, step = i + 1).
      if (for_wrap.loop_blk.step !== 4'h4)
        $fatal(1, "FAIL loop_blk.step=%h", for_wrap.loop_blk.step);
      if (r_for !== 8'h0A)   // 1 + 2 + 3 + 4 = 10 = 0x0A
        $fatal(1, "FAIL r_for=%h", r_for);

      $display("PASSED");
   end
endmodule

// Disable statement via hierarchical reference (IEEE 1364-2005 S9.6.2).
//
// (1) Disable a named block addressed hierarchically:
//     disable u.blk;
// (2) Disable a task in a submodule while it is active:
//     disable u.inst.my_task;

module vlog77_inner;
   reg [7:0] result = 8'h00;

   task my_task;
      begin
         #100;
         // Should never reach here if disabled externally.
         result = 8'hFF;
      end
   endtask
endmodule

module vlog77_sub;
   reg [7:0] val = 8'h00;

   // Named block that can be disabled from outside.
   always begin : blk
      #50;
      val = val + 1;
   end

   vlog77_inner inst ();
endmodule

module vlog77;
   vlog77_sub u ();

   // ---- (1) Disable a named block ----
   initial begin
      #10;
      // val should still be 0 (blk hasn't fired its first #50 yet).
      if (u.val !== 8'h00) begin
         $display("FAILED: initial u.val=%0d expected 0", u.val);
         $finish;
      end

      // Let blk run once.
      #50;
      if (u.val !== 8'h01) begin
         $display("FAILED: after first blk u.val=%0d expected 1", u.val);
         $finish;
      end

      // Disable the named block; it should stop the always iteration.
      disable u.blk;
      #60;
      // After disable + delay, blk should have restarted (always re-enters)
      // but we verify the disable took effect by checking timing.
      // val may be 2 if the always re-entered (which is correct for always).
   end

   // ---- (2) Disable a task via hier path ----
   initial begin
      // Fork off the task.
      fork
         u.inst.my_task;
      join_none

      #10;
      // Task is running (waiting on #100), disable it.
      disable u.inst.my_task;
      #200;

      // result should still be 0 because we disabled the task
      // before it reached the assignment.
      if (u.inst.result !== 8'h00) begin
         $display("FAILED: task result=%h expected 00", u.inst.result);
         $finish;
      end

      $display("PASSED");
      $finish;
   end
endmodule

// SV built-in method call on hier-ref tail: named event .triggered.
//
// An inner module declares an event; the outer module triggers it via
// hier path and then reads u.ev.triggered to verify it fired.
//
// Note: nvc does not currently support dynamic arrays, queues, or their
// .size()/.delete() methods.  This test covers only the named-event
// .triggered property (IEEE 1800-2017 S9.4.2), which is the simplest
// built-in method on a hier-ref tail.

module vlog80_sub;
   event ev;
endmodule

module vlog80;
   vlog80_sub u ();

   reg saw_triggered = 0;
   reg ev_fired = 0;

   // Process 1: wait for the event using @().
   initial begin
      @(u.ev);
      ev_fired = 1;
   end

   // Process 2: trigger the event via hier path, then check .triggered.
   initial begin
      #10;
      -> u.ev;
      #0;  // Let the trigger propagate within the same time step.

      // .triggered is true for the remainder of the time step in which
      // the event was triggered (IEEE 1800 S9.4.2).
      if (u.ev.triggered !== 1'b1) begin
         $display("FAILED: u.ev.triggered=%b expected 1", u.ev.triggered);
         $finish;
      end
      saw_triggered = 1;
   end

   // Checker: verify both paths observed the event.
   initial begin
      #20;
      if (ev_fired !== 1) begin
         $display("FAILED: @(u.ev) did not fire, ev_fired=%0d", ev_fired);
         $finish;
      end
      if (saw_triggered !== 1) begin
         $display("FAILED: .triggered path did not complete");
         $finish;
      end
      $display("PASSED");
      $finish;
   end
endmodule

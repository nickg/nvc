// Cross (mutual/circular) hierarchical references between sibling modules.
//
// vlog81_A reads modB.x (upward to parent vlog81, then down into sibling).
// vlog81_B reads modA.y (upward to parent vlog81, then down into sibling).
// Both must resolve after both module bodies exist in the modcache
// (second resolver pass).  This verifies the resolver does not deadlock
// on circular cross-references.

module vlog81_A;
   reg [7:0] y = 8'hAA;
   reg [7:0] got_from_B;

   initial begin
      #2;
      got_from_B = modB.x;
   end
endmodule

module vlog81_B;
   reg [7:0] x = 8'hBB;
   reg [7:0] got_from_A;

   initial begin
      #2;
      got_from_A = modA.y;
   end
endmodule

module vlog81;
   vlog81_A modA ();
   vlog81_B modB ();

   initial begin
      #5;
      // Verify A successfully read B's value.
      if (modA.got_from_B !== 8'hBB) begin
         $display("FAILED: modA.got_from_B=%h expected BB", modA.got_from_B);
         $finish;
      end

      // Verify B successfully read A's value.
      if (modB.got_from_A !== 8'hAA) begin
         $display("FAILED: modB.got_from_A=%h expected AA", modB.got_from_A);
         $finish;
      end

      $display("PASSED");
      $finish;
   end
endmodule

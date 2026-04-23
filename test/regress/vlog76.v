// Op-assign, increment, and decrement via hierarchical references.
//
// SystemVerilog operator-assignment and pre/post-increment/decrement
// with hier-ref lvalues.
//
// (a) u.x += 1;     (op-assign)
// (b) u.y++;        (post-increment)
// (c) --u.u2.z;     (pre-decrement)

module vlog76_leaf;
   reg [7:0] z = 8'd10;
endmodule

module vlog76_sub;
   reg [7:0] x = 8'd5;
   reg [7:0] y = 8'd20;
   vlog76_leaf u2 ();
endmodule

module vlog76;
   vlog76_sub u ();

   initial begin
      #1;

      // ---- (a) op-assign ----
      u.x += 1;
      #1;
      if (u.x !== 8'd6) begin
         $display("FAILED: u.x += 1 => %0d expected 6", u.x);
         $finish;
      end

      u.x -= 3;
      #1;
      if (u.x !== 8'd3) begin
         $display("FAILED: u.x -= 3 => %0d expected 3", u.x);
         $finish;
      end

      // ---- (b) post-increment ----
      u.y++;
      #1;
      if (u.y !== 8'd21) begin
         $display("FAILED: u.y++ => %0d expected 21", u.y);
         $finish;
      end

      // ---- (c) pre-decrement through two levels ----
      --u.u2.z;
      #1;
      if (u.u2.z !== 8'd9) begin
         $display("FAILED: --u.u2.z => %0d expected 9", u.u2.z);
         $finish;
      end

      $display("PASSED");
      $finish;
   end
endmodule

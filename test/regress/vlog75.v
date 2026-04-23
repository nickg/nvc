// Force and release through hierarchical references.
//
// (a) force/release on a reg target via hier ref.
// (b) force/release on a net (wire) target via hier ref.
// (c) force/release with tail bit-select: force u.u2.vec[3] = 1'b1;
//
// IEEE 1364-2005 S9.3.

module vlog75_leaf;
   reg [7:0] vec = 8'h00;
endmodule

module vlog75_sub;
   reg [7:0] x = 8'h00;
   wire [7:0] w;
   reg [7:0] w_driver = 8'h00;
   assign w = w_driver;

   vlog75_leaf u2 ();
endmodule

module vlog75;
   vlog75_sub u ();

   initial begin
      // ---- (a) force / release on a reg ----
      u.x = 8'hAA;
      #1;
      force u.x = 8'hFF;
      #1;
      if (u.x !== 8'hFF) begin
         $display("FAILED: force reg u.x=%h expected FF", u.x);
         $finish;
      end
      release u.x;
      #1;
      // After release, reg retains forced value until next procedural write.
      if (u.x !== 8'hFF) begin
         $display("FAILED: after release reg u.x=%h expected FF", u.x);
         $finish;
      end
      u.x = 8'hBB;
      #1;
      if (u.x !== 8'hBB) begin
         $display("FAILED: post-release write u.x=%h expected BB", u.x);
         $finish;
      end

      // ---- (b) force / release on a net ----
      u.w_driver = 8'h11;
      #1;
      if (u.w !== 8'h11) begin
         $display("FAILED: net u.w=%h expected 11", u.w);
         $finish;
      end
      force u.w = 8'h22;
      #1;
      if (u.w !== 8'h22) begin
         $display("FAILED: force net u.w=%h expected 22", u.w);
         $finish;
      end
      release u.w;
      #1;
      // After release, net reverts to its driver.
      if (u.w !== 8'h11) begin
         $display("FAILED: after release net u.w=%h expected 11", u.w);
         $finish;
      end

      // ---- (c) force / release with tail bit-select ----
      u.u2.vec = 8'h00;
      #1;
      force u.u2.vec[3] = 1'b1;
      #1;
      if (u.u2.vec !== 8'h08) begin
         $display("FAILED: force bit u.u2.vec=%h expected 08", u.u2.vec);
         $finish;
      end
      release u.u2.vec[3];
      #1;
      if (u.u2.vec[3] !== 1'b1) begin
         $display("FAILED: after release bit u.u2.vec[3]=%b expected 1",
                  u.u2.vec[3]);
         $finish;
      end

      // ---- (d) deassign after procedural continuous assign ----
      // Skipped: nvc does not support procedural deassign statements
      // (being considered for removal from the SV standard).

      $display("PASSED");
      $finish;
   end
endmodule

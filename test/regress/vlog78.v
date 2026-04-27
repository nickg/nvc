// Bit select, part select, and indexed part select on hierarchical
// references, used as both RHS (read) and LHS (write).
//
// IEEE 1364-2005 S4.2.1 bit select, S4.2.2 part select,
// SystemVerilog indexed part select +: / -:.

module vlog78_sub;
   reg [15:0] x = 16'hA5C3;
endmodule

module vlog78;
   vlog78_sub u ();

   reg        bit_val;
   reg [7:0]  part_val;
   reg [3:0]  ipart_p;
   reg [3:0]  ipart_m;
   integer    i;

   initial begin
      // ---- RHS reads ----

      // Bit select
      bit_val = u.x[3];
      if (bit_val !== 1'b0) begin
         $display("FAILED: u.x[3]=%b expected 0 (x=%h)", bit_val, u.x);
         $finish;
      end

      // Part select
      part_val = u.x[7:0];
      if (part_val !== 8'hC3) begin
         $display("FAILED: u.x[7:0]=%h expected C3", part_val);
         $finish;
      end

      // Indexed part select +: (u.x[4+:4] = bits [7:4] = 4'hC)
      i = 4;
      ipart_p = u.x[i+:4];
      if (ipart_p !== 4'hC) begin
         $display("FAILED: u.x[%0d+:4]=%h expected C", i, ipart_p);
         $finish;
      end

      // Indexed part select -: (u.x[11-:4] = bits [11:8] = 4'h5)
      i = 11;
      ipart_m = u.x[i-:4];
      if (ipart_m !== 4'h5) begin
         $display("FAILED: u.x[%0d-:4]=%h expected 5", i, ipart_m);
         $finish;
      end

      // ---- LHS writes ----

      // Bit select LHS
      u.x[0] = 1'b0;
      #1;
      if (u.x[0] !== 1'b0) begin
         $display("FAILED: LHS bit u.x[0]=%b expected 0", u.x[0]);
         $finish;
      end

      // Part select LHS
      u.x[7:0] = 8'hFF;
      #1;
      if (u.x[7:0] !== 8'hFF) begin
         $display("FAILED: LHS part u.x[7:0]=%h expected FF", u.x[7:0]);
         $finish;
      end

      // Indexed part select +: LHS (write bits [11:8])
      i = 8;
      u.x[i+:4] = 4'h0;
      #1;
      if (u.x[11:8] !== 4'h0) begin
         $display("FAILED: LHS +: u.x[11:8]=%h expected 0", u.x[11:8]);
         $finish;
      end

      // Indexed part select -: LHS (write bits [15:12])
      i = 15;
      u.x[i-:4] = 4'h3;
      #1;
      if (u.x[15:12] !== 4'h3) begin
         $display("FAILED: LHS -: u.x[15:12]=%h expected 3", u.x[15:12]);
         $finish;
      end

      $display("PASSED");
      $finish;
   end
endmodule

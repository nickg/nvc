// Multi-dimensional chained selects, nested packed-array member access,
// and replication with hierarchical reference operands.
//
// (a) u.mem[addr][7:0]  — multi-dim chain with part select
// (b) u.grid[i][j]      — 2-D array index
// (c) u.pkt[31:24]      — nested packed array with part select
//     (Note: SV struct member syntax u.pkt.hdr.sof is not used here because
//      nvc's struct support is limited.  Instead we use a packed array and
//      access sub-fields via part-select, which exercises the same
//      composition-on-hier-ref-base path.)
// (d) {4{u.x}}          — replication with hier operand

module vlog79_sub;
   reg [15:0] mem [0:3];   // 4-entry memory, 16 bits each
   reg [7:0]  grid [0:1][0:1];  // 2x2 grid of bytes
   reg [31:0] pkt;         // packed "struct": [31:24]=hdr, [23:0]=payload
   reg [3:0]  x;

   initial begin
      mem[0] = 16'h1234;
      mem[1] = 16'h5678;
      mem[2] = 16'h9ABC;
      mem[3] = 16'hDEF0;
      grid[0][0] = 8'hAA;
      grid[0][1] = 8'hBB;
      grid[1][0] = 8'hCC;
      grid[1][1] = 8'hDD;
      pkt = 32'hDE_AD_BE_EF;
      x = 4'hA;
   end
endmodule

module vlog79;
   vlog79_sub u ();

   reg [7:0]  low_byte;
   reg [7:0]  cell;
   reg [7:0]  hdr;
   reg [15:0] replicated;
   integer    addr, i, j;

   initial begin
      #1;

      // ---- (a) multi-dim chain with part select ----
      addr = 1;
      low_byte = u.mem[addr][7:0];
      if (low_byte !== 8'h78) begin
         $display("FAILED: u.mem[%0d][7:0]=%h expected 78", addr, low_byte);
         $finish;
      end

      // ---- (b) 2-D array index ----
      i = 1;
      j = 0;
      cell = u.grid[i][j];
      if (cell !== 8'hCC) begin
         $display("FAILED: u.grid[%0d][%0d]=%h expected CC", i, j, cell);
         $finish;
      end

      // ---- (c) packed array part select (struct substitute) ----
      // pkt = 32'hDEADBEEF => [31:24] = 8'hDE
      hdr = u.pkt[31:24];
      if (hdr !== 8'hDE) begin
         $display("FAILED: u.pkt[31:24]=%h expected DE", hdr);
         $finish;
      end

      // ---- (d) replication with hier operand ----
      // x = 4'hA => {4{4'hA}} = 16'hAAAA
      replicated = {4{u.x}};
      if (replicated !== 16'hAAAA) begin
         $display("FAILED: {4{u.x}}=%h expected AAAA", replicated);
         $finish;
      end

      // ---- write through multi-dim hier ref ----
      u.mem[2][7:0] = 8'hFF;
      #1;
      if (u.mem[2] !== 16'h9AFF) begin
         $display("FAILED: write u.mem[2]=%h expected 9AFF", u.mem[2]);
         $finish;
      end

      u.grid[0][1] = 8'h42;
      #1;
      if (u.grid[0][1] !== 8'h42) begin
         $display("FAILED: write u.grid[0][1]=%h expected 42", u.grid[0][1]);
         $finish;
      end

      $display("PASSED");
      $finish;
   end
endmodule

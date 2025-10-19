module simp1;
  localparam p0 = 5;
  localparam p1 = p0 & 3;   // 1
  localparam p2 = p0 | 3;   // 7
  localparam p3 = p1 < p2;  // 1
  localparam p4 = p0 ^ 7;   // 2
endmodule // simp1

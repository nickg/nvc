module simp1;
  localparam p0 = 5;
  localparam p1 = p0 & 3;   // 1
  localparam p2 = p0 | 3;   // 7
  localparam p3 = p1 < p2;  // 1
  localparam p4 = p0 ^ 7;   // 2
  localparam p5 = p0 ? 2 : 1;  // 2
  localparam signed [3:0] p6 = -4'sd4;
  localparam [3:0] p7 = p6 >> 1;   // 6
  localparam signed [3:0] p8 = p6 >>> 1;  // -2
  localparam p9 = p6 + 0;  // -4
endmodule // simp1

module simp2;
  localparam p0 = 5;
  localparam p1 = 1'bx ? 1 : 0;  // X
  localparam p2 = 6 / 0;         // X
  localparam p3 = 16'h1234;
endmodule // simp2

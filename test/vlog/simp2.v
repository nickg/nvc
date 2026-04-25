module simp2;
  localparam p0 = 5;
  localparam p1 = 1'bx ? 1 : 0;  // Does not simplify
  localparam p2 = 6 / 0;         // X
endmodule // simp2

module mod1;
  wire x;
  if (x) begin  // Error
  end
  localparam p = 5;
  if (p) begin    // OK
    mod2 u1();
  end else begin
    mod2 u1();    // OK
  end
  if (p) g1: begin: g1  // Error
  end : g2   // Error
  if (p) begin end : g2 // Error
endmodule // mod1

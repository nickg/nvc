module param1;
  parameter [7:0] p1 = 8'd5;  // OK
  assign w1 = p1;   // OK
  parameter logic p1 = 1;   // Error
  localparam bit  p2 = 0;
  assign w2 = !p2; // OK
endmodule // param1

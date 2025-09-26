module param2 # (
  logic [7:0] p1 = 8'd5,
  parameter p2 = 8,
  p3 = 7,
  localparam bit p4 = 0,
  p5 = 5,
  parameter p6 = 4
) (
  input x, y,
  output reg z
);
  assign w1 = p1;   // OK
  parameter logic p1 = 1;   // Error
endmodule // param2

module mod2 #(p1);
  parameter p2 = 5;   // OK (really a localparam)
  parameter p3;   // Error
endmodule

module mod3;
  localparam p_triple = (5:6:7);
endmodule

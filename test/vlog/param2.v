module param2 # (
  [7:0] p1 = 8'd5,
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

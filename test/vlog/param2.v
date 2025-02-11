module param2 #(
  parameter [7:0] p1 = 8'd5,
  parameter p2 = 8
);
  assign w1 = p1;   // OK
  parameter logic p1 = 1;   // Error
endmodule // param2

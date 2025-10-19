module const1;
  parameter int p1 = 5;
  wire [p1-1:0] w1; // OK
  wire [w1-1:0] w2; // Error
  wire [+p1:6]  w3; // OK
  wire [p1[0]:0] w4; // OK
  wire [7:-1] w5; // OK
  wire [p1 > 3 ? 6 : p1+2:0] w6; // OK
  reg [$clog2(p1)/2:0] r1; // OK
  localparam p2 = p1 & 3;  // OK
endmodule // const1

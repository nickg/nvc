module const1;
  parameter int p1 = 5;
  wire [p1-1:0] w1; // OK
  wire [w1-1:0] w2; // Error
  wire [+p1:6]  w3; // OK
  wire [p1[0]:0] w4; // OK
  wire [7:-1] w5; // OK
endmodule // const1

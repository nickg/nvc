module lower2;
  reg [7:0] x;
  reg [3:0] y1, y2, y3;

  initial y1 = x[4:1];

  initial y2 = x[2:-1];

endmodule // lower2

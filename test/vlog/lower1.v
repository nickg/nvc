module lower1;
  wire x, y, z;
  assign z = x & y;

  reg r;
  always @(*)
    r = !x;

endmodule // lower1

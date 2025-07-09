module sub1(o);
  output o;
  parameter X = -5;

  if (X > 0)
    assign o = 1;
  else if (X < 0)
    assign o = 0;

endmodule // sub1

module sub2(o);
  output o;
  parameter X = -5;

  if (X >= 0)
    assign o = 1;
  else if (X <= -5)
    assign o = 0;

endmodule // sub2

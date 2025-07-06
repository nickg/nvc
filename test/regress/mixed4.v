module extend(i, o);
  parameter WIDTH;
  input i;
  output [WIDTH-1:0] o;

  assign o = {WIDTH{i}};
endmodule

module combine(i, o);
  parameter WIDTH;
  input [WIDTH-1:0] i;
  output o;

  assign o = |i;
endmodule

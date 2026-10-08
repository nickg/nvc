module issue1681_sub (
  input logic [7:0] a,
  output wire pass
);
  assign pass = a == 8'h2a;
endmodule

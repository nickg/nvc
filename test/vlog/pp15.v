`define ASSIGN(lhs, rhs) assign lhs = rhs;

module pp15;
  wire [1:0] in;
  wire [1:0] out;

  `ASSIGN(out, {in[0], in[1]})
endmodule

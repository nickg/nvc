module ports (input next, input [3:0] in, output [3:0] out);

  assign out = next ? in : 4'b0;

endmodule // ports

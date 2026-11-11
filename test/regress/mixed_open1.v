module open_mod #(parameter N = 3)
   (output reg [7:0] o);
  initial o = 10 + N;
endmodule

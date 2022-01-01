module dff (input d, clk, rstb,
            output reg q);
  always @(posedge clk)
    q <= d;
endmodule // dff

module duplicate_x (input x, x /* error */);
endmodule // duplicate_x

module bad_ref1 (input d, clk, rstb,
                 output reg q);
  always @(posedge clk)
    qq /* error */ <= dd /* error */;
endmodule // dff

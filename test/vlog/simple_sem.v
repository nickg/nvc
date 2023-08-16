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

module bad_target (input d, output q, output reg r);
  //reg x;
  wire [7:0] bus;
  assign r = 1; // Error
  initial q <= 5; // Error

endmodule // bad_target

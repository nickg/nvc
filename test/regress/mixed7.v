module sub(clk_in, din, dout);
  input clk_in;
  input [7:0] din;
  output reg [7:0] dout;
  wire clk_delay1 = clk_in;
  wire clk_delay2;

  assign clk_delay2 = clk_delay1;

  always @(posedge clk_delay2)
    dout <= din;

endmodule // sub

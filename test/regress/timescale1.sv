`timescale 1ns/1fs

module sub(output reg [7:0] x);
  initial begin
    x = 0;
    #1;
    x = 1;
    #3;
    x = 2;
    #1ms;
    x = 3;
  end
endmodule // sub

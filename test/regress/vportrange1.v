// Port and net declarations whose ranges depend on parameters
module vportrange1_sub (a, y);
  parameter W = 4;
  parameter IW = (W > 2) ? W : 2;
  input  [W-1:0] a;
  output [W-1:0] y;
  wire   [IW-1:0] y;
  assign y = ~a;
endmodule

module vportrange1;
  reg  [3:0] a = 4'b0101;
  wire [3:0] y;

  vportrange1_sub #(.W(4)) u(a, y);

  initial begin
    #1;
    if (y !== 4'b1010) $display("FAILED %b", y);
    else $display("PASSED");
  end
endmodule

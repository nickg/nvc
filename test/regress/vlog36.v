// Test for module with parameter port list and non-ANSI port declarations
module vlog36_dut
#(parameter WIDTH = 8)
(
    clk,
    rst,
    din,
    dout
);

input clk;
input rst;
input [WIDTH-1:0] din;
output reg [WIDTH-1:0] dout;

always @(posedge clk)
begin
    if (rst)
        dout <= 0;
    else
        dout <= din;
end

endmodule

module vlog36;
  reg clk = 0;
  reg rst = 1;
  reg [7:0] din = 0;
  wire [7:0] dout;

  vlog36_dut #(.WIDTH(8)) dut (
    .clk(clk),
    .rst(rst),
    .din(din),
    .dout(dout)
  );

  initial begin
    #1 clk = 1; #1 clk = 0;
    rst = 0;
    din = 8'd42;
    #1 clk = 1; #1 clk = 0;
    $display("dout=%0d", dout);
    if (dout !== 8'd42)
      $display("FAIL: expected 42");
    din = 8'd99;
    #1 clk = 1; #1 clk = 0;
    $display("dout=%0d", dout);
    if (dout !== 8'd99)
      $display("FAIL: expected 99");
    $finish;
  end

endmodule

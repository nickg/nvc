`timescale 1fs/1fs
module sub(x);
  output reg [7:0] x;
  reg        y, tmp;

  initial begin
    x = 0;
    #1;
    $display("first");
    y = 1;
    @(x);
    y = !y;
    @(x);
    y = !y;
    @(x);
    y = !y;
    tmp = y;
    y <= !y;
    #0;
    $display("last");
    if (y !== tmp) begin
      $display("y changed too soon");
      $fatal;
    end
  end

  always @(y) begin
    $display("y changed ==> %x", y);
    x = x + 1;
  end

endmodule // sub

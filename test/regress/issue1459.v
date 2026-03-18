// This would hang during elaboration
module issue1459;
  reg [3:0] x;
  wire [3:0] y;
  wire [1:0] z;

  assign {y, z} = {x, x[1:0]};

  initial begin
    x = 4'b1010;
    #1;
    $display("PASSED");
  end
endmodule

module alu (x, y, add);
  parameter WIDTH = 32;
  input [WIDTH-1:0] x, y;
  output [WIDTH-1:0] add;

  assign add = x + y;
endmodule // alu

module wide1;
  reg [99:0] x, y;
  wire [99:0] add;

  alu #(100) u(x, y, add);

  initial begin
    x = 1;
    y = 2;
    #1;
    x = 7;
    #1;
    x = 64'hffffffffffffffff;
  end

  always @(*)
    $display("%x %x | %x", x, y, add);

endmodule // wide1

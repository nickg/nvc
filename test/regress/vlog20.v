module adder(a, b, o);
  parameter WIDTH = 16;
  input [WIDTH-1:0] a, b;
  output [WIDTH-1:0] o;
  wire [WIDTH-1:0] o;

  assign o = a + b;

  always @(*)
    $display("%x + %x ==> %x", a, b, o);

endmodule // adder

module vlog20;
  wire [7:0] o;
  reg [7:0]  a, b;

  adder #(8) u(a, b, o);

  integer i;
  initial begin
    for (i = 0; i < 10; i++) begin
      a = i;
      b = i * 5;
      #1;
    end
  end

endmodule // vlog20

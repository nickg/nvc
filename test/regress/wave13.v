module wave13;
  reg [7:0] x;
  reg       y;
  wire      z;
  wire [3:0] q;

  always @(z)
    y <= !z;

  assign z = ^x;

  assign q = x + 1;

  integer   i;
  initial begin
    x = 0;
    for (i = 0; i < 5; i++)
      #1 x = x + 1;
    $display("PASSED");
  end

endmodule // wave13

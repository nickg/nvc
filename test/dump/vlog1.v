module dff (input d, clk, rstb,
            output reg q);
  always @(posedge clk)
    q <= d;
endmodule // dff

module mod2;
  wire [7:0] bus;
  reg        r;
  initial begin
    $display("hello");
    if (bus)
      r <= 1 | r;
    $finish;
    r = 1;
    #1 r <= 0;
  end
  assign bus = 3;
endmodule // mod2

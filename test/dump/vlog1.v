module dff (input d, clk, rstb,
            output reg q);
  always @(posedge clk)
    q <= d;
endmodule // dff

module mod2;
  wire [7:0] bus;
  initial begin
    $display("hello");
    $finish;
  end
  assign bus = 3;
endmodule // mod2

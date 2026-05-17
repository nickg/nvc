module select22;
  reg clk = 0;
  reg [31:0] q = 0;
  reg failed = 0;

  always @(posedge clk)
    {q[31:25], q[11:7]} <= 12'habc;

  initial begin
    #1 clk = 1;
    #1;

    if (q[31:25] !== 7'h55)
      failed = 1;

    if (q[11:7] !== 5'h1c)
      failed = 1;

    if (failed)
      $display("FAILED");
    else
      $display("PASSED");
  end
endmodule

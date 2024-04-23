module vlog9;
  reg clk;
  reg [7:0] cnt;

  initial begin
    cnt = 0;
    clk = 1'b0;
    forever #1 clk = ~clk;
  end

  always @(posedge clk)
    cnt = cnt + 1;

  initial begin
    #20;
    if (cnt === 10)
      $display("PASSED");
    else
      $display("FAILED -- %d", cnt);
    $finish;
  end

endmodule // vlog9

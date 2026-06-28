module vlog44;
  reg clk = 1'b0;
  reg a = 1'b0;
  reg y;

  always #1 clk = ~clk;

  always @*
    y = !a;

  always @(posedge clk) begin
    a <= 1'bx;
    a <= 1'b0;
  end

  initial begin
    repeat (2) @(posedge clk);
    #1;

    if (y === 1'b1)
      $display("PASSED");
    else
      $display("FAILED");

    $finish;
  end
endmodule

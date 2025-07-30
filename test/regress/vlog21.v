module vlog21;
  reg x, y;
  integer i;

  initial begin
    x = 0;
    y = 0;
    #1;
    x <= 1;  // Must be scheduled after for-loop below
    for (i = 0; i < 10; i++)
      y = !y;
    if (x === 0)
      $display("PASSED");
    else
      $display("FAILED -- %x", x);
  end
endmodule // vlog20

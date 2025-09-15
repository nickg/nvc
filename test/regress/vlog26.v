module vlog26;
  reg x, y, z;

  initial begin
    {x, y, z} = 0;
    #1;
    x = 1;
  end

  always @* y = x;
  always @* z = y;

  initial begin
    #1;
    #0;
    if (z !== 1)
      $display("FAILED -- %x", z);
    else
      $display("PASSED");
  end

endmodule // vlog26

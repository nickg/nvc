module vlog16;
  reg [7:0] x, cnt;

  always @(x[4]) begin
    $display("x ==> %x (%d)", x, ++cnt);
  end

  initial begin
    cnt = 0;
    #1 x = 2;
    #1 x = 3;
    #1 x = 8;
    #1 x = 16;
    #1 x = 17;
    #1 x = 100;
    #1;

    if (cnt === 3)
      $display("PASSED");
    else
      $display("FAILED -- %d != 3", cnt);
  end

endmodule // vlog16

module wide2;

  reg signed [7:0] n1;
  reg signed [80:0] w1;
  reg signed [100:0] w2;

  initial begin
    n1 = -7;
    w1 = n1;
    w2 = w1;
    #1;
    $display("n1=%x w1=%x w2=%x", n1, w1, w2);
    if (w1 !== 81'h1fffffffffffffffffff9 || w2 !== 101'h1ffffffffffffffffffffffff9)
      $display("FAILED");
    else
      $display("PASSED");
  end

endmodule // wide2

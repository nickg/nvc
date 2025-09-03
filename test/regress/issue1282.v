module issue1282;
  reg i, fail = 0;
  wire o1;

  buf (o1, i);

  initial begin
    i = 1;
    #1;
    $display("%b", o1);
    if (o1 !== 1) fail = 1;
    i = 1'bz;
    #1;
    $display("%b", o1);
    if (o1 !== 1'bx) fail = 1;
    #1;
    if (fail)
      $display("FAILED");
    else
      $display("PASSED");
  end

endmodule // issue1282

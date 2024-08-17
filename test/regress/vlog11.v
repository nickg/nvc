module vlog11;
  reg i1, i2;

  nand (o1, i1, i2);
  xor (o2, i1, i2);

  initial begin
    i1 = 1;
    i2 = 0;
    #1 $display("%d %d", o1, o2);
    if (o1 !== 1) $display("FAILED");
    if (o2 !== 1) $display("FAILED");

    i1 = 0;
    i2 = 1;
    #1 $display("%d %d", o1, o2);
    if (o1 !== 1) $display("FAILED");
    if (o2 !== 1) $display("FAILED");

    i1 = 1;
    i2 = 1;
    #1 $display("%d %d", o1, o2);
    if (o1 !== 0) $display("FAILED");
    if (o2 !== 0) $display("FAILED");

    i1 = 0;
    i2 = 0;
    #1 $display("%d %d", o1, o2);
    if (o1 !== 1) $display("FAILED");
    if (o2 !== 0) $display("FAILED");

    //i1 = 1;
    //i2 = 1'bx;
    //#1 $display("%d", o1);
    //if (o1 !== 1'bx) $display("FAILED");

    $display("PASSED");
  end

endmodule // vlog10

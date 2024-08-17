module vlog10;
  reg i1, i4, i5;

  and (o, i1, i2);
  or (i2, i3, i4);
  not (i3, i5);

  initial begin
    i5 = 1;
    i4 = 0;
    i1 = 1;
    #1 $display("%d", o);
    if (o !== 0) $display("FAILED");

    i5 = 0;
    i4 = 1;
    i1 = 0;
    #1 $display("%d", o);
    if (o !== 0) $display("FAILED");

    i5 = 1;
    i4 = 1;
    i1 = 1;
    #1 $display("%d", o);
    if (o !== 1) $display("FAILED");

    i5 = 1;
    i4 = 1;
    i1 = 0;
    #1 $display("%d", o);
    if (o !== 0) $display("FAILED");

    i5 = 0;
    i4 = 1;
    i1 = 1;
    #1 $display("%d", o);
    if (o !== 1) $display("FAILED");

    $display("PASSED");
  end

endmodule // vlog10

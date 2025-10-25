module binary4;
  integer x, y, z;
  reg signed [126:0] a, b, c;
  reg     pass = 1;

  initial begin
    x = 42;
    y = 3;
    #1;
    z = x >>> y;
    #1;
    $display("%d >>> %d = %d", x, y, z);
    pass &= (z === 5);

    x = -42;
    y = 3;
    #1;
    z = x >>> y;
    #1;
    $display("%d >>> %d = %d", x, y, z);
    pass &= (z === -6);

    z = x <<< -x;
    #1;
    $display("%d <<< %d = %d", x, -x, z);
    pass &= (z === 0);

    a = 42;
    b = 3;
    #1;
    c = a >>> b;
    #1;
    $display("%d >>> %d = %d", a, b, c);
    pass &= (c === 5);

    a = -42;
    b = 3;
    #1;
    c = a >>> b;
    #1;
    $display("%d >>> %d = %x", a, b, c);
    pass &= (c === -6);

    if (pass)
      $display("PASSED");
    else
      $display("FAILED");
  end

endmodule // binary4

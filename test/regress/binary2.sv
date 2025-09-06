module binary2;
  byte x = 42;
  bit  failed = 0;

  initial begin
    x += 1;
    x -= 2;
    #1;
    if (x !== 41) failed = 1;

    x <<= 1;
    x >>= 2;
    #1;
    if (x !== 20) failed = 1;

    x <<<= 5;
    x >>>= 3;
    #1;
    if (x !== -16) failed = 1;

    x *= 100;
    #1;
    if (x != 8'b11000000) failed = 1;

    x |= 5;
    x &= 8'hf;
    #1;
    if (x !== 5) failed = 1;

    x ^= -1;
    #1;
    if (x !== -6) failed = 1;

    $display("%s", failed ? "FAILED" : "PASSED");
  end

endmodule // binary2

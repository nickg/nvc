module signed4;

  reg passed;

  reg signed [7:0] a8, b8, q8, r8;
  reg signed [80:0] a81, b81, q81, r81;

  initial begin
    passed = 1'b1;

    a8 = -8'sd7;
    b8 = 8'sd2;
    q8 = a8 / b8;
    r8 = a8 % b8;
    if (q8 !== -8'sd3 || r8 !== -8'sd1) begin
      $display("FAILED: 8-bit -7 / 2 gave q=%0d r=%0d", q8, r8);
      passed = 1'b0;
    end

    a8 = 8'sd7;
    b8 = -8'sd2;
    q8 = a8 / b8;
    r8 = a8 % b8;
    if (q8 !== -8'sd3 || r8 !== 8'sd1) begin
      $display("FAILED: 8-bit 7 / -2 gave q=%0d r=%0d", q8, r8);
      passed = 1'b0;
    end

    a81 = -81'sd7;
    b81 = 81'sd2;
    q81 = a81 / b81;
    r81 = a81 % b81;
    if (q81 !== -81'sd3 || r81 !== -81'sd1) begin
      $display("FAILED: 81-bit -7 / 2 gave q=%0d r=%0d", q81, r81);
      passed = 1'b0;
    end

    a81 = 81'sd7;
    b81 = -81'sd2;
    q81 = a81 / b81;
    r81 = a81 % b81;
    if (q81 !== -81'sd3 || r81 !== 81'sd1) begin
      $display("FAILED: 81-bit 7 / -2 gave q=%0d r=%0d", q81, r81);
      passed = 1'b0;
    end

    if (passed)
      $display("PASSED");
    else
      $display("FAILED");
  end

endmodule

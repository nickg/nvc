module wide6;

  reg passed;
  reg [80:0] a, b, c;
  reg signed [80:0] sa, sb;

  initial begin
    passed = 1'b1;

    a = 81'h1_0000_0000_0000_0005;
    b = 81'h0_0000_0000_0000_0002;
    c = a - b;
    if (c !== 81'h1_0000_0000_0000_0003) begin
      $display("FAILED: wide subtract gave %h", c);
      passed = 1'b0;
    end

    if ((a && b) !== 1'b1) begin
      $display("FAILED: wide logical and");
      passed = 1'b0;
    end

    b = 81'd0;
    if ((a || b) !== 1'b1) begin
      $display("FAILED: wide logical or true");
      passed = 1'b0;
    end

    a = 81'd0;
    if ((a || b) !== 1'b0) begin
      $display("FAILED: wide logical or false");
      passed = 1'b0;
    end

    a = 81'h0_0000_0000_0000_0007;
    b = 81'h0_0000_0000_0000_0009;
    if ((a < b) !== 1'b1 || (a <= b) !== 1'b1 ||
        (b > a) !== 1'b1 || (b >= a) !== 1'b1) begin
      $display("FAILED: wide unsigned relational");
      passed = 1'b0;
    end

    sa = -81'sd7;
    sb = 81'sd3;
    if ((sa < sb) !== 1'b1 || (sa <= sb) !== 1'b1 ||
        (sb > sa) !== 1'b1 || (sb >= sa) !== 1'b1) begin
      $display("FAILED: wide signed relational");
      passed = 1'b0;
    end

    if (passed)
      $display("PASSED");
  end

endmodule

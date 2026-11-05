module vlog50;

  real r;
  reg signed [7:0] s;
  integer i;
  reg [7:0] u;

  // A signed integral operand must be converted to real as a signed
  // value when the other operand is real.
  initial begin
    s = -3;
    i = -5;
    u = 250;

    r = 1.5 + s;
    if (r != -1.5) begin
      $display("FAILED: 1.5 + s = %f", r);
      $fatal;
    end

    r = s * 2.0;
    if (r != -6.0) begin
      $display("FAILED: s * 2.0 = %f", r);
      $fatal;
    end

    r = i + 0.5;
    if (r != -4.5) begin
      $display("FAILED: i + 0.5 = %f", r);
      $fatal;
    end

    if (!(s < 0.5)) begin
      $display("FAILED: s < 0.5");
      $fatal;
    end

    r = u + 0.5;    // Unsigned operand still zero extended
    if (r != 250.5) begin
      $display("FAILED: u + 0.5 = %f", r);
      $fatal;
    end

    $display("PASSED");
    $finish;
  end

endmodule // vlog50

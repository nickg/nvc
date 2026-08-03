// Mixed signedness bitwise operands must be made unsigned before widening.
module binary11;
  reg signed [3:0] s;
  reg        [7:0] u;
  reg        [7:0] r;
  reg              fail;

  initial begin
    fail = 1'b0;
    s = 4'b1000;
    u = 8'b10100101;

    r = s | u;
    $display("s | u = %b", r);
    if (r !== 8'b10101101) fail = 1'b1;

    r = u | s;
    $display("u | s = %b", r);
    if (r !== 8'b10101101) fail = 1'b1;

    r = s & u;
    $display("s & u = %b", r);
    if (r !== 8'b00000000) fail = 1'b1;

    r = u & s;
    $display("u & s = %b", r);
    if (r !== 8'b00000000) fail = 1'b1;

    r = s ^ u;
    $display("s ^ u = %b", r);
    if (r !== 8'b10101101) fail = 1'b1;

    r = u ^ s;
    $display("u ^ s = %b", r);
    if (r !== 8'b10101101) fail = 1'b1;

    if (fail)
      $display("FAILED");
    else
      $display("PASSED");
  end
endmodule

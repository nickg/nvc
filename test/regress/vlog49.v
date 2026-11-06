module vlog49;

  reg signed [7:0] s;

  // The negated literal is shared between the two $display calls.
  // Truncating it for the assignment to s must not modify the value
  // seen by the second call.
  initial begin
    $display("%0d", -4);
    s = -4;
    $display("%0d", -4);
    $display("%0d", ~3);
    s = ~3;
    $display("%0d", ~3);
  end

endmodule // vlog49

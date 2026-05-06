module real9;
  real r;
  integer a, b;

  initial begin
    a = 32'hff;
    b = 32'd42;
    r = a & b;
    $display("%f", r);
    if (r == 42.0)
      $display("PASSED");
    else
      $display("FAILED");
  end

endmodule // real9

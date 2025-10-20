module real8;
  real r1, r2;
  reg  pass = 1;

  initial begin
    r1 = 1.0;
    r2 = 0.5;
    r1 += r2;
    #1;
    if (r1 != 1.5) begin
      $display("%f != 1.5", r1);
      pass = 0;
    end

    r1 = r1 * 2.0;
    #1;
    if (r1 != 3.0) begin
      $display("%f != 3.0", r1);
      pass = 0;
    end

    if (pass)
      $display("PASSED");
    else
      $display("FAILED");
  end

endmodule // real8

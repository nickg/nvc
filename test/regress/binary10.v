module binary10;

  reg pass;
  reg [3:0] a;
  reg [31:0] s32;
  reg signed [3:0] sa;
  reg [39:0] r;

  initial begin
    pass = 1'b1;

    a = 4'hf;
    s32 = 32'd1;
    sa = -4'sd2;
    #1;

    r = { a << s32, 4'h0 };
    if (r !== 40'h00000000e0) begin
      $display("FAILED: shift widened by RHS width gave %h", r);
      pass = 1'b0;
    end

    r = { sa >>> s32, 4'h0 };
    if (r !== 40'h00000000f0) begin
      $display("FAILED: signed arithmetic shift gave %h", r);
      pass = 1'b0;
    end

    if (pass)
      $display("PASSED");
    else
      $display("FAILED");
  end

endmodule

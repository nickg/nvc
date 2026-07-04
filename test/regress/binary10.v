module binary10;

  reg pass;
  reg [3:0] a;
  reg [31:0] s32;
  reg signed [3:0] sa;
  reg [39:0] r;
  reg        b;
  reg        grant_valid;
  reg        grant_encoded;
  reg [1:0] ready_vec;

  initial begin
    pass = 1'b1;

    a = 4'hf;
    s32 = 32'd1;
    sa = -4'sd2;
    b = 1'b1;
    grant_valid = 1'b1;
    grant_encoded = 1'b1;
    #1;

    /*
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

    a = 1'b1 << 1;
    if (a !== 2'b10) begin
      $display("FAILED: 1'b1 << 1 ==> %b", a);
      pass = 0;
    end

    a = b << 1;
    if (a !== 2'b10) begin
      $display("FAILED: %b << 1 ==> %b", b, a);
      pass = 0;
    end
     */

    ready_vec = (b && grant_valid) << grant_encoded;
    if (ready_vec !== 2'b10) begin
      $display("FAILED: (%b && %b) << %b ==> %b",
               b, grant_valid, grant_encoded, ready_vec);
      pass = 0;
    end

    if (pass)
      $display("PASSED");
    else
      $display("FAILED");
  end

endmodule

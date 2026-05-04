module binary6;
  reg [63:0] u_hi, u_lo, u_same;
  reg signed [63:0] s_hi, s_lo;
  reg [7:0] result;
  reg pass = 1'b1;

  initial begin
    u_hi = 64'h8000_0000_0000_0000;
    u_lo = 64'h0000_0000_0000_0001;
    u_same = 64'h8000_0000_0000_0000;
    s_hi = 64'h8000_0000_0000_0000;
    s_lo = 64'h0000_0000_0000_0001;
    #1;

    result = { u_hi > u_lo, u_hi >= u_lo, u_hi < u_lo, u_hi <= u_lo,
               u_lo > u_hi, u_lo >= u_hi, u_lo < u_hi, u_lo <= u_hi };
    $display("unsigned_cross_sign=%b", result);
    if (result !== 8'b11000011)
      pass = 1'b0;

    result = { u_hi > u_same, u_hi >= u_same, u_hi < u_same, u_hi <= u_same,
               u_hi == u_same, u_hi != u_same, u_hi == u_lo, u_hi != u_lo };
    $display("unsigned_equal=%b", result);
    if (result !== 8'b01011001)
      pass = 1'b0;

    result = { s_hi > s_lo, s_hi >= s_lo, s_hi < s_lo, s_hi <= s_lo,
               s_lo > s_hi, s_lo >= s_hi, s_lo < s_hi, s_lo <= s_hi };
    $display("signed_cross_sign=%b", result);
    if (result !== 8'b00111100)
      pass = 1'b0;

    if (pass)
      $display("PASSED");
    else
      $display("FAILED");
  end
endmodule // binary6

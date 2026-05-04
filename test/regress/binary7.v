module binary7;
  reg pass;
  reg [7:0] u8_a, u8_b;
  reg signed [7:0] s8_a, s8_b;
  reg [7:0] u8_c, u8_d;
  reg [15:0] u16;
  reg signed [15:0] s16;

  initial begin
    pass = 1'b1;

    u8_a = 8'h00;
    u8_b = 8'd2;
    u16 = 16'd1000;
    #1;

    // If RHS is incorrectly evaluated at 8 bits: (~0)/2 = 127 and this is true.
    // Correct context-determined evaluation at 16 bits gives 32767 and false.
    if (u16 > ((~u8_a) / u8_b)) begin
      $display("FAILED unsigned (~a)/b width-extension");
      pass = 1'b0;
    end

    u8_c = 8'h00;
    u8_d = 8'd2;
    u16 = 16'd1000;
    #1;

    // Mirror: LHS context-determined expression should be evaluated at 16 bits.
    // Incorrect 8-bit evaluation gives 127 <= 1000 (true); correct is 32767 <= 1000 (false).
    if (((~u8_c) / u8_d) <= u16) begin
      $display("FAILED unsigned LHS (~a)/b width-extension");
      pass = 1'b0;
    end

    u8_c = 8'h00;
    u8_d = 8'd1;
    u16 = 16'd1000;
    #1;

    // LHS context propagation with a different operator pair (~ and -):
    // Incorrect 8-bit eval gives 254 < 1000 (true); correct 16-bit gives 65534 < 1000 (false).
    if ((((~u8_c) - u8_d) < u16)) begin
      $display("FAILED unsigned LHS (~a)-b width-extension");
      pass = 1'b0;
    end

    s8_a = 8'sh80;   // -128
    s8_b = 8'sd2;
    s16 = 16'sd0;
    #1;

    // If RHS is incorrectly evaluated at 8 bits: (-(-128))/2 = -64 and this is false.
    // Correct context-determined evaluation at 16 bits gives 64 and true.
    if (!(s16 < ((-s8_a) / s8_b))) begin
      $display("FAILED signed (-a)/b width-extension");
      pass = 1'b0;
    end

    if (pass)
      $display("PASSED");
    else
      $display("FAILED");
  end
endmodule

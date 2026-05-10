module wide7;

  reg passed;
  reg [80:0] a, b, r;

  initial begin
    passed = 1'b1;

    a = { 17'h00000, 64'hffff_0000_ffff_0000 };
    b = { 17'h00001, 64'h00ff_00ff_00ff_00ff };
    r = a ~& b;
    if (r !== { 17'h1ffff, 64'hff00_ffff_ff00_ffff }) begin
      $display("FAILED: wide nand known gave %h", r);
      passed = 1'b0;
   end

    r = a ~| b;
    if (r !== { 17'h1fffe, 64'h0000_ff00_0000_ff00 }) begin
      $display("FAILED: wide nor known gave %h", r);
      passed = 1'b0;
    end

    a = { 17'h12345, 1'bx, 63'h0123_4567_89ab_cdef };
    b = { 17'h00800, 1'b1, 63'h0011_1111_1111_1111 };
    r = a ~& b;
    if (r !== { 17'h1ffff, 1'bx, 63'h7ffe_fefe_fefe_fefe }) begin
      $display("FAILED: wide nand x gave %h", r);
      passed = 1'b0;
   end

    a = { 17'h12345, 1'bx, 63'h0123_4567_89ab_cdef };
    b = { 17'h00800, 1'b0, 63'h0011_1111_1111_1111 };
    r = a ~| b;
    if (r !== { 17'h0d4ba, 1'bx, 63'h7ecc_aa88_6644_2200 }) begin
      $display("FAILED: wide nor x gave %h", r);
      passed = 1'b0;
   end

    if (passed)
      $display("PASSED");
  end

endmodule

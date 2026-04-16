module wide5;

  reg passed;
  reg [80:0] value;
  reg [1:0] result;

  initial begin
    passed = 1'b1;

    value = { 17'h12345, 1'bx, 63'h0123_4567_89ab_cdef };
    result = 2'b00;
    casex (value)
      { 17'h12345, 1'b0, 63'h0123_4567_89ab_cdef }: result = 2'b01;
      default: result = 2'b10;
    endcase
    if (result !== 2'b01) begin
      $display("FAILED: wide casex");
      passed = 1'b0;
    end

    value = { 17'h12345, 1'bz, 63'h0123_4567_89ab_cdef };
    result = 2'b00;
    casez (value)
      { 17'h12345, 1'b0, 63'h0123_4567_89ab_cdef }: result = 2'b01;
      default: result = 2'b10;
    endcase
    if (result !== 2'b01) begin
      $display("FAILED: wide casez z");
      passed = 1'b0;
    end

    value = { 17'h12345, 1'bx, 63'h0123_4567_89ab_cdef };
    result = 2'b00;
    casez (value)
      { 17'h12345, 1'b0, 63'h0123_4567_89ab_cdef }: result = 2'b01;
      default: result = 2'b10;
    endcase
    if (result !== 2'b10) begin
      $display("FAILED: wide casez x");
      passed = 1'b0;
    end

    if (passed)
      $display("PASSED");
  end

endmodule

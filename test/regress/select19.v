module select19;
  reg [15:0] value = 16'h0123;
  reg [3:0] nibble;
  reg bit_value;

  initial begin
    #1;

    nibble = value[(1'bx)+:4];
    if (nibble !== 4'bxxxx) begin
      $display("FAILED: unknown +: base selected %b", nibble);
      $finish;
    end

    nibble = value[(1'bx)-:4];
    if (nibble !== 4'bxxxx) begin
      $display("FAILED: unknown -: base selected %b", nibble);
      $finish;
    end

    bit_value = value[1'bx];
    if (bit_value !== 1'bx) begin
      $display("FAILED: unknown bit index selected %b", bit_value);
      $finish;
    end

    $display("PASSED");
  end
endmodule

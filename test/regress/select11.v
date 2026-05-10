module select11;
  reg [7:0] x;
  integer bit_count = 0;
  integer part_count = 0;

  always @(x[8])
    bit_count = bit_count + 1;

  always @(x[8:7])
    part_count = part_count + 1;

  initial begin
    x = 8'h00;
    #1;

    if (bit_count !== 0 || part_count !== 1) begin
      $display("FAILED (1) -- %0d %0d", bit_count, part_count);
      $finish;
    end

    x[6] = 1'b1;
    #1;

    if (bit_count !== 0 || part_count !== 1) begin
      $display("FAILED (2) -- %0d %0d", bit_count, part_count);
      $finish;
    end

    x[7] = 1'b1;
    #1;

    if (bit_count !== 0 || part_count !== 2) begin
      $display("FAILED (3) -- %0d %0d", bit_count, part_count);
      $finish;
    end

    $display("PASSED");
  end
endmodule // select11

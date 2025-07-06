module select7;
  reg [7:0] x;
  wire [1:0] y;

  assign y = x[5:4];
  assign z = x[5:4];   // Implicit net

  always @(x) $display("===> %x", x);

  initial begin
    x[7:0] = 8'h42;
    #1 if (y !== 2'b0) begin
      $display("FAILED (1) -- %x", y);
      $finish;
    end

    x[5:3] = 3'b111;
    #1 if (y !== 2'b11) begin
      $display("FAILED (2) -- %x", y);
      $finish;
    end

    if (z !== 1) begin
      $display("FAILED (3) -- %x", z);
      $finish;
    end

    $display("PASSED");
  end

endmodule // select7

module select2();
  reg [67:60] x;
  reg [1:0] y;
  reg       b;

  initial begin
    x = 42;

    y = x[61:60];
    if (y !== 2) begin
      $display("FAILED (1) -- %x !== 2", y);
      $finish;
    end

    y = x[66:65];
    if (y !== 1) begin
      $display("FAILED (2) -- %x !== 1", y);
      $finish;
    end

    $display("PASSED");
    $finish;
  end

endmodule // vlog15

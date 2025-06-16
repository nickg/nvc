module vlog15();
  reg [7:0] x;
  reg       b;

  // Out-of-range bit select
  initial begin
    x = 42;

    b = x[1];
    if (b !== 1) begin
      $display("FAILED (1) -- %d !== 1", b);
      $finish;
    end

    b = x[8];
    if (b !== 1'bx) begin
      $display("FAILED (2) -- %d !== 1", b);
      $finish;
    end

    b = x[123512632];
    if (b !== 1'bx) begin
      $display("FAILED (3) -- %d !== 1", b);
      $finish;
    end

    $display("PASSED");
    $finish;
  end

endmodule // vlog15

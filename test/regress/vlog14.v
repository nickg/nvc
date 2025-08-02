module vlog14;
  reg [7:0] x, y, z, q;

  always @(*) x <= y + z;

  always @* q <= 1; // Should never execute

  initial begin
    y <= 5;
    z <= 2;
    #1;
    if (x !== 7) begin
      $display("FAILED: %d !== 7", x);
      $finish;
    end
    z <= 10;
    #1;
    if (x !== 15) begin
      $display("FAILED: %d !== 15", x);
      $finish;
    end
    #1;
    if (q == 1) begin
      $display("FAILED: %d == 1 (%x)", q, q == 1);
      $finish;
    end
    $display("PASSED");
  end

endmodule // vlog14

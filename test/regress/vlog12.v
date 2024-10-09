module vlog12;
  reg x, y, z;
  reg [7:0] cnt;

  always @(x or y or posedge z)
    cnt <= cnt + 1;

  initial begin
    cnt = 0;
    x <= 0;
    y <= 0;
    z <= 0;
    #1;
    if (cnt !== 1) begin
      $display("FAILED: %d !== 1", cnt);
      $finish;
    end
    z <= 1;
    #1;
    if (cnt !== 2) begin
      $display("FAILED: %d !== 2", cnt);
      $finish;
    end
    z <= 0;
    #1;
    if (cnt !== 2) begin
      $display("FAILED: %d !== 2", cnt);
      $finish;
    end
    y <= 1;
    #1;
    if (cnt !== 3) begin
      $display("FAILED: %d !== 3", cnt);
      $finish;
    end
    $display("PASSED");
  end

endmodule // vlog12

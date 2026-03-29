module signed3;
  reg        [7:0] a = 8'b11111111;
  reg signed [7:0] b = 8'b11111111;

  initial begin
    if (
      a < 0 ||
      $unsigned(a) < 0 ||
      $signed(a) > 0 ||
      b > 0 ||
      $unsigned(b) < 0 ||
      $signed(b) > 0
    ) begin
      $display("FAILED");
    end
    else begin
      $display("PASSED");
    end
  end

endmodule // signed3

module vlog15;
  reg [7:0] x;
  reg [3:0] y;

  initial begin
    y = 4'h2;
    x = { 4'h1, y };
    if (x !== 8'h12) begin
      $display("FAILED (1) -- %x !== 12", x);
      $finish;
    end
    x = { y, x[7:4] };
    if (x !== 8'h21) begin
      $display("FAILED (2) -- %x !== 21", x);
      $finish;
    end
    $display("PASSED");
  end
endmodule // vlog15

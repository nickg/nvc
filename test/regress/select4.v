module select4;
  reg [8:1] x = 0;
  reg [15:0] idx;

  initial begin
    x[0] = 1;
    if (x !== 0) begin
      $display("FAILED (1) -- %x !== 0", x);
      $finish;
    end

    x[8] = 1;
    if (x !== 8'h80) begin
      $display("FAILED (2) -- %x !== 80", x);
      $finish;
    end

    idx = 10000;
    #1 x[idx] = 1;
    if (x !== 8'h80) begin
      $display("FAILED (3) -- %x !== 80", x);
      $finish;
    end

    $display("PASSED");
  end
endmodule // select4

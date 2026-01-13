module vlog30;
  reg clk;
  integer pos, neg;

  always @(posedge clk) pos++;
  always @(negedge clk) neg++;

  initial begin
    pos = 0;
    neg = 0;
    clk = 1'bx;

    #1 clk = 0;
    #0 if (pos !== 0 || neg !== 1) begin
      $display("FAILED (1) : pos=%d neg=%d", pos, neg);
      $finish;
    end

    #1 clk = 1;
    #0 if (pos !== 1 || neg !== 1) begin
      $display("FAILED (2) : pos=%d neg=%d", pos, neg);
      $finish;
    end

    #1 clk = 0;
    #0 if (pos !== 1 || neg !== 2) begin
      $display("FAILED (3) : pos=%d neg=%d", pos, neg);
      $finish;
    end

    #1 clk = 1'bz;
    #0 if (pos !== 2 || neg !== 2) begin
      $display("FAILED (4) : pos=%d neg=%d", pos, neg);
      $finish;
    end

    #1 clk = 1'bx;
    #0 if (pos !== 2 || neg !== 2) begin
      $display("FAILED (5) : pos=%d neg=%d", pos, neg);
      $finish;
    end

    #1 clk = 1'b1;
    #0 if (pos !== 3 || neg !== 2) begin
      $display("FAILED (6) : pos=%d neg=%d", pos, neg);
      $finish;
    end

    #1 clk = 1'bx;
    #0 if (pos !== 3 || neg !== 3) begin
      $display("FAILED (7) : pos=%d neg=%d", pos, neg);
      $finish;
    end

    $display("PASSED");
  end

endmodule // vlog30

module select12;
  reg [7:0] x;

  initial begin
    x = 8'hff;

    x[8] <= 1'b0;
    #1;

    if (x !== 8'hff) begin
      $display("FAILED (1) -- %b", x);
      $finish;
    end

    x[8:7] <= 2'b10;
    #1;

    if (x !== 8'h7f) begin
      $display("FAILED (2) -- %b", x);
      $finish;
    end

    x[8:7] <= 2'b11;
    #1;

    if (x !== 8'hff) begin
      $display("FAILED (3) -- %b", x);
      $finish;
    end

    $display("PASSED");
  end
endmodule // select12

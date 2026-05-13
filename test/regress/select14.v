module select14;
  reg a, b;
  wire [7:0] x;

  buf (x[8], a);
  buf (x[8:7], b);

  initial begin
    a = 1'b0;
    b = 1'b0;
    #1;

    if (x !== 8'b0zzzzzzz) begin
      $display("FAILED (1) -- %b", x);
      $finish;
    end

    a = 1'b1;
    #1;

    if (x !== 8'b0zzzzzzz) begin
      $display("FAILED (2) -- %b", x);
      $finish;
    end

    b = 1'b1;
    #1;

    if (x !== 8'b1zzzzzzz) begin
      $display("FAILED (3) -- %b", x);
      $finish;
    end

    $display("PASSED");
  end
endmodule // select14

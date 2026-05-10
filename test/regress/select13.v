module select13;
  reg a;
  reg [1:0] b;
  wire [7:0] x;

  assign x[8] = a;
  assign x[8:7] = b;

  initial begin
    a = 1'b0;
    b = 2'b10;
    #1;

    if (x !== 8'b0xxxxxxx) begin
      $display("FAILED (1) -- %b", x);
      $finish;
    end

    a = 1'b1;
    #1;

    if (x !== 8'b0xxxxxxx) begin
      $display("FAILED (2) -- %b", x);
      $finish;
    end

    b = 2'b11;
    #1;

    if (x !== 8'b1xxxxxxx) begin
      $display("FAILED (3) -- %b", x);
      $finish;
    end

    $display("PASSED");
  end
endmodule // select13

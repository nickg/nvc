module select16_sub(input i, output o);
  assign o = i;
endmodule

module select16;
  reg a;
  wire [7:0] x;

  select16_sub u(.i(a), .o(x[8]));

  initial begin
    a = 1'b0;
    #1;

    if (x !== 8'bzzzzzzzz) begin
      $display("FAILED (1) -- %b", x);
      $finish;
    end

    a = 1'b1;
    #1;

    if (x !== 8'bzzzzzzzz) begin
      $display("FAILED (2) -- %b", x);
      $finish;
    end

    $display("PASSED");
  end
endmodule // select16

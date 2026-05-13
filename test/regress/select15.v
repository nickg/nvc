module select15_sub2(input [1:0] i, output [1:0] o);
  assign o = i;
endmodule

module select15;
  reg [1:0] b;
  wire [7:0] x;

  select15_sub2 u2(.i(b), .o({x[8:7]}));

  initial begin
    b = 2'b10;
    #1;

    if (x !== 8'b0zzzzzzz) begin
      $display("FAILED (1) -- %b", x);
      $finish;
    end

    b = 2'b11;
    #1;

    if (x !== 8'b1zzzzzzz) begin
      $display("FAILED (2) -- %b", x);
      $finish;
    end

    $display("PASSED");
  end
endmodule // select15

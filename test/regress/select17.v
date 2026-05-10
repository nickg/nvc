module select17_sub(input [1:0] i, output [1:0] o);
  assign o = i;
endmodule

module select17;
  reg [1:0] b;
  wire [7:0] x;

  select17_sub u(.i(b), .o(x[8:7]));

  initial begin
    b = 2'b10;
    #1;

    if (x !== 8'b0xxxxxxx) begin
      $display("FAILED (1) -- %b", x);
      $finish;
    end

    b = 2'b11;
    #1;

    if (x !== 8'b1xxxxxxx) begin
      $display("FAILED (2) -- %b", x);
      $finish;
    end

    $display("PASSED");
  end
endmodule // select17

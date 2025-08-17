module vlog24;
  wire [2:0] a;
  wire       b;
  reg [7:0]  x;

  assign {a, b} = x;

  initial begin
    x = 8'b00001011;
    #1;
    $display("%b %b", a, b);
    if (a !== 3'b101 || b !== 1'b1)
      $display("FAILED");
    else
      $display("PASSED");
    $finish;
  end

endmodule // vlog24

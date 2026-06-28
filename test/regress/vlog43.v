module sub(input wire [31:0] a, input wire b);
  reg seen = 1'b0;

  always @(a, b)
    seen = 1'b1;
endmodule

module vlog43;
  localparam [31:0] P = 32'hc0a80180;
  reg               kick = 0;

  sub u (.a(P), .b(kick));

  initial begin
    #1;
    kick = 1;
    #1;
    if (u.seen === 1'b1)
      $display("PASSED");
    else
      $display("FAILED");
    $finish;
  end
endmodule // vlog43

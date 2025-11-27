module unary1;
  wire [7:0] x = 8'b01001100, y = 0;
  reg r1, r2, r3;

  initial begin
    #1;  // TODO: shouldn't be necessary?
    r1 = ~&(x);
    r2 = ~|(x);
    r3 = ^(+x);
    $display("%b %b %b", r1, r2, r3);
    if (r1 !== 1 || r2 !== 0) begin
      $display("FAILED");
      $finish;
    end
    $display("PASSED");
    $finish;
  end

endmodule // unary1

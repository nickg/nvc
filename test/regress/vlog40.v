module vlog40;
  integer x = 0;
  integer n = 0;

  always @(n)
    repeat (n) begin
      x = x + 1;
      #1;
    end

  initial begin
    #1;
    n = 5;
    #1;
    n = 7;
    #5;
    n = 1;
    #2;
    if (x === 6)
      $display("PASSED");
    else
      $display("FAILED");
  end

endmodule // vlog40

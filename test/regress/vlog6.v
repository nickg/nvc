module vlog6;
  reg x;
  reg [7:0] y;

  initial begin
    x <= 0;
    y <= 0;
    #1;
    $display("%x %x %x %x", !x, ~x, !y, ~y);
    x <= 1;
    y <= 128;
    #0.1;   // Rounds to zero
    $display("%x %x %x %x", !x, ~x, !y, ~y);
    #1;
    $display("%x %x %x %x", !x, ~x, !y, ~y);
    $finish;
  end

endmodule // vlog6

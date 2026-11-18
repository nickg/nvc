module vrealbits1;
  real r, back;
  reg [63:0] bits;
  initial begin
    r = 1.5;
    bits = $realtobits(r);
    $display("%h", bits);
    back = $bitstoreal(bits);
    $display("%f", back);
    bits = $realtobits(-2.25);
    $display("%h", bits);
    $display("%f", $bitstoreal(64'h4059000000000000));
    if (back != 1.5 || $bitstoreal($realtobits(0.1)) != 0.1)
      $display("FAILED");
  end
endmodule

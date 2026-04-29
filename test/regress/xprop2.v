module xprop2;
  reg [70:0] a, b, and_r, or_r, xor_r, not_r;

  initial begin
    a = 71'b0101xx01101x;
    b = 71'bx1001x0xx100;
    #1;

    and_r = a & b;
    or_r = a | b;
    xor_r = a ^ b;
    not_r = !a;

    $display("and=%b or=%b xor=%b not=%b", and_r, or_r, xor_r, not_r);

    if (and_r !== 71'b000100xx0xx000
        || or_r != 71'bxxxx1011x01111x
        || xor_r != 71'bxxxx001xx0xx11x
        || not_r != 71'b0)
      $display("FAILED");
    else
      $display("PASSED");
  end
endmodule // xprop2

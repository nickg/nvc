module xprop2;
  reg [70:0] a, b, and_r, or_r, xor_r, not_r, bitnot_x_r, bitnot_z_r;

  initial begin
    a = 71'b0101xx01101x;
    b = 71'bx1001x0xx100;
    #1;

    and_r = a & b;
    or_r = a | b;
    xor_r = a ^ b;
    not_r = !a;
    bitnot_x_r = ~{71{1'bx}};
    bitnot_z_r = ~{71{1'bz}};

    $display("and=%b or=%b xor=%b not=%b bitnot_x=%b bitnot_z=%b",
             and_r, or_r, xor_r, not_r, bitnot_x_r, bitnot_z_r);

    if (and_r !== 71'b000100xx0xx000
        || or_r != 71'bxxxx1011x01111x
        || xor_r != 71'bxxxx001xx0xx11x
        || not_r != 71'b0
        || bitnot_x_r !== {71{1'bx}}
        || bitnot_z_r !== {71{1'bx}})
      $display("FAILED");
    else
      $display("PASSED");
  end
endmodule // xprop2

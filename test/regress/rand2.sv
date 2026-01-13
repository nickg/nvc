module rand2;
  int accum;

  initial begin
    $display("%x", $random);
    accum += $random;
    accum += $random;
    accum += $random;
    #1;

    $display("%x", accum);

    // Assumes --seed=123
    if (accum != 32'h82582603)
      $display("FAILED");
    else
      $display("PASSED");
  end

endmodule // rand2

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
    if (accum != 32'h363f9f6b)
      $display("FAILED");
    else
      $display("PASSED");
  end

endmodule // rand2

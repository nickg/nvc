module rand4;
  initial begin
    $display($random, $random, $random);

    // Assumes --seed=123
    if ($random != -1969134571)
      $display("FAILED (1)");
    else if ($random != -1732201935)
      $display("FAILED (2)");
    else if ($random != -862267751)
      $display("FAILED (3)");
    else
      $display("PASSED");
  end
endmodule // rand4

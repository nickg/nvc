module rand4;
  reg [39:0] wide;

  initial begin
    $display($random, $random, $random);

    // Assumes --seed=123
    if ($random != -1969134571)
      $display("FAILED (1)");
    else if ($random != -1732201935)
      $display("FAILED (2)");
    else if ($random != -862267751)
      $display("FAILED (3)");
    else begin
      wide = $random;
      if (wide === 40'hff8ab67c15)
        $display("PASSED");
      else
        $display("FAILED (4) %x", wide);
    end
  end
endmodule // rand4

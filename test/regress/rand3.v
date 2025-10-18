module rand3;
  integer seed = 0;
  integer x1, y1, x2, y2;

  initial begin
    x1 = $random(seed);
    y1 = $random(seed);
    $display("%x %x %x", x1, y1, seed && seed !== 0);
    seed = 0;
    x2 = $random(seed);
    y2 = $random(seed);
    $display("%x %x %x", x2, y2, seed);

    if (x1 === x2 && y1 == y2 && seed !== 0)
      $display("PASSED");
    else
      $display("FAILED");
  end

endmodule

module vlog23;
  reg [7:0] i;
  initial begin
    i = 0;
    repeat (5) i++;
    while (i < 100)
      i = i * 2;  // *= 2
    $display("%d", i);
    if (i === 160)
      $display("PASSED");
    else
      $display("FAILED");
    $finish;
  end
endmodule // vlog23

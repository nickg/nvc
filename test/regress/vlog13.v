module vlog13;
  wire x = 1, y = 1;
  assign y = 0;

  initial begin
    #1;
    if (x !== 1)
      $display("FAILED -- x=%d", x);
    else if (y === 0 || y === 1)
      $display("FAILED -- y=%d", y);
    else
      $display("PASSED");
  end
endmodule // vlog13

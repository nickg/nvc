module string1;
  reg [4*8-1:0] str;
  reg           failed = 0;

  initial begin
    str = "abc\"";
    if (str !== 32'h61626322) failed = 1;
    str = "a\x99\0\0";
    if (str !== 32'h61990000) failed = 1;
    str = "\n\t\\\v";
    if (str !== 32'h0a095c0b) failed = 1;

    if (failed)
      $display("FAILED");
    else
      $display("PASSED");
  end

endmodule // string1

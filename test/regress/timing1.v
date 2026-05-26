module timing1;

  integer value;

  initial begin
    value = (2:10:17);

    if (value != 10)
      $display("FAILED: value=%0d", value);
    else
      $display("PASSED");

    $finish;
  end

endmodule

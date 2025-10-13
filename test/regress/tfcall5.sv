module tfcall5;
  int x;

  task task1(input int val);
    #3;
    x = val;
  endtask // task1

  initial begin
    task1(42);

    #0 if ($time === 3 && x === 42)
      $display("PASSED");
    else
      $display("FAILED");
  end

  always @* $display("%t %x", $time, x);

endmodule // tfcall5

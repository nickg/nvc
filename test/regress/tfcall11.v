module tfcall11;
  function integer fact_iter;
    input integer n;
    begin
      fact_iter = 1;
      while (n > 1) begin
        fact_iter *= n;
        n = n - 1;
      end
    end
  endfunction // fact_iter

  integer res;
  reg     fail = 0;

  initial begin
    res = fact_iter(5);
    $display("fact_iter(5) ==> %d (120)", res);
    if (res !== 120) fail = 1;
    res = fact_iter(0);
    $display("fact_iter(0) ==> %d (1)", res);
    if (res !== 1) fail = 1;
    if (fail)
      $display("FAILED");
    else
      $display("PASSED");
  end
endmodule // tfcall11

module issue1680;
  localparam P = 0;

  if (1) begin: gen_f
    integer base = 10;

    function integer add_one(input integer n);
      add_one = n + 1;
    endfunction

    function integer add_base(input integer n);
      add_base = n + base;   // Uses state in the generate block
    endfunction

    task set_base(input integer n);
      base = n;
    endtask

    if (1) begin: inner
      function integer twice(input integer n);
        twice = 2 * n;
      endfunction
    end
  end

  if (P) begin: g2
    integer x = 1;
  end
  else begin: g2
    integer x = 2;
  end

  reg fail = 0;

  initial begin
    #1;
    if (gen_f.add_one(1) !== 2) fail = 1;
    if (gen_f.add_base(5) !== 15) fail = 1;
    gen_f.set_base(20);
    if (gen_f.base !== 20) fail = 1;
    if (gen_f.add_base(5) !== 25) fail = 1;
    if (g2.x !== 2) fail = 1;
    if (gen_f.inner.twice(21) !== 42) fail = 1;
    if (fail)
      $display("FAILED");
    else
      $display("PASSED");
  end
endmodule

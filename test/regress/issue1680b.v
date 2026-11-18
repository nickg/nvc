module issue1680b;
  localparam P = 0;

  if (P) begin: g
    reg x;
  end
  else begin: g
    function integer f(input integer a);
      f = a + 7;
    endfunction

    if (1) begin: a
      if (1) begin: b
        integer y = 0;
        task t;
          y = 5;
        endtask
        function integer h(input integer n);
          h = n * 3;
        endfunction
      end
    end
  end

  reg fail = 0;

  initial begin
    #1;
    if (g.f(1) !== 8) fail = 1;
    g.a.b.t;
    if (g.a.b.y !== 5) fail = 1;
    if (g.a.b.h(4) !== 12) fail = 1;
    if (fail)
      $display("FAILED");
    else
      $display("PASSED");
  end
endmodule

module href4;
  if (1) begin: g
    function integer f(input integer n);
      f = n;
    endfunction
  end

  initial begin
    $display(g.nothere(1));     // Error
    $display(nonesuch.f(1));    // Error
  end

  if (1) begin: other
    initial $display(g.f(1));   // Error
  end

endmodule // href4

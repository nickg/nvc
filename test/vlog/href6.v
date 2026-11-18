module href6;
  if (1) begin: g
    function integer f(input integer n);
      f = n + 1;
    endfunction
  end

  localparam P = g.f(1);   // Error
endmodule // href6

module vlog52;
  function integer f(input integer a);
    f = a + 1;
  endfunction

  reg [7:0] x = 8'hab;

  initial begin
    $display("%0d", f(3));
    $display("%h", 16'(x));
    $display("%0d", f(f(1)) + 1);
  end
endmodule // vlog52

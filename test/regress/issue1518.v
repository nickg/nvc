module issue1518;

  function integer my_log2;
    input integer n;
    integer       i;
    begin
      my_log2 = 0;
      for (i = n - 1; i > 0; i = i >> 1)
        my_log2 = my_log2 + 1;
    end
  endfunction

  parameter integer DEPTH     = 256;
  parameter integer ADDR_BITS = my_log2(DEPTH);

  reg [ADDR_BITS-1:0] addr;

  initial begin
    $display("DEPTH     = %0d", DEPTH);
    $display("ADDR_BITS = %0d", ADDR_BITS);
    addr = ADDR_BITS[ADDR_BITS-1:0];
    $display("addr = %0d", addr);
    if (addr === 8)
      $display("PASSED");
    else
      $display("FAILED");
    $finish;
  end

endmodule // issue1518

module constfunc1;
  function integer my_log2;
    input integer n;
    integer       i;
    begin
      my_log2 = 0;
      for (i = n - 1; i > 0; i = i >> 1)
        my_log2 = my_log2 + 1;
    end
  endfunction // my_log2

  parameter integer DEPTH     = 256;
  parameter integer ADDR_BITS = my_log2(DEPTH);  // OK

  integer           i;
  parameter integer BAD1 = my_log2(i);  // Error

  function integer get_i;
    get_i = i;
  endfunction // get_i

  localparam integer BAD2 = my_log2(get_i());  // Error
  localparam real    NAN = $sqrt(-1.0);  // OK
  localparam integer OK1 = my_log2($signed(DEPTH));  // OK
  localparam integer BAD3 = $signed(i);  // Error

endmodule // constfunc1

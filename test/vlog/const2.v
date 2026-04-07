// Test for constant user function calls in expressions
module const2;
  parameter WIDTH = 8;

  function integer clog2;
    input integer value;
    integer i;
    begin
      clog2 = 0;
      for (i = value - 1; i > 0; i = i >> 1)
        clog2 = clog2 + 1;
    end
  endfunction

  function integer sum_bits;
    input [7:0] mask;
    integer i;
    integer count;
    begin
      count = 0;
      for (i = 0; i < 8; i = i + 1)
        if (mask[i])
          count = count + 1;
      sum_bits = count;
    end
  endfunction

  localparam BITS = clog2(WIDTH);       // Should be 3
  wire [BITS-1:0] w1;                   // OK: 3-bit wire
  wire [clog2(16)-1:0] w2;             // OK: 4-bit wire

  localparam N_ACTIVE = sum_bits(8'b10110010);  // Should be 4
  wire [N_ACTIVE-1:0] w3;              // OK: 4-bit wire

  wire [sum_bits(8'b0):0] w4;          // OK: constant function in part-select

  wire [clog2(w1):0] w5;               // Error: non-constant argument
endmodule

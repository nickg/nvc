module tfcall3;
  wire [1+2+3+4-1:0] out;

  genvar i;
  for (i = 1; i <= 4; i = i + 1) begin
    function [i-1:0] get_bits;
      input b;
      begin
        $display("get_bits %d", i);
        get_bits = {(i){b}};
      end
    endfunction // get_bits
    wire [i-1:0] bits;
    localparam   base = ((i - 1) * i) / 2;
    assign bits = get_bits(i & 1);
    assign out[base+i-1:base] = bits;
  end // for (i = 1; i <= 4; i = i + 1)

  initial begin
    #1;
    $display("%b", out);
    if (out === 'b0000111001)
      $display("PASSED");
    else
      $display("FAILED");
    $finish;
  end

endmodule // tfcall3

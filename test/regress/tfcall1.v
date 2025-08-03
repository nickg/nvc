module tfcall1;

  function [15:0] swap_bytes;
    input [15:0] x;
    swap_bytes = { x[7:0], x[15:8] };
  endfunction // swap_bytes

  reg [15:0] result;

  initial begin
    result = swap_bytes(16'h1234);
    if (result !== 16'h3412) begin
      $display("FAILED -- %x !== 3412", result);
      $finish;
    end
    $display("PASSED");
  end

endmodule // tfcall1

`timescale 1fs/1fs
module sub(i, o);
  output [7:0] o;
  input [7:0]  i;
  wire [7:0]   tmp1, tmp2;

  function [7:0] trace;
    input [7:0] x;
    begin
      $display("trace ==> %x", x);
      trace = x;
    end
  endfunction

  assign tmp1 = trace(i + 1);
  assign tmp2 = trace(tmp1 + 5);
  assign o = trace(tmp2 - 5);

  always @(i) begin
    $display("i changed ==> %x", i);
  end

endmodule // sub

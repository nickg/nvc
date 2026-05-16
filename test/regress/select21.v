// Test procedural concatenation assignment to multiple local variables.
module select21;

function [3:0] get_hi(input [7:0] value);
  reg [3:0] hi;
  reg [3:0] lo;
  begin
    {hi, lo} = value;
    get_hi = hi;
  end
endfunction

function [3:0] get_lo(input [7:0] value);
  reg [3:0] hi;
  reg [3:0] lo;
  begin
    {hi, lo} = value;
    get_lo = lo;
  end
endfunction

reg failed = 0;

initial begin
  if (get_hi(8'ha5) !== 4'ha)
    failed = 1;

  if (get_lo(8'ha5) !== 4'h5)
    failed = 1;

  if (failed)
    $display("FAILED");
  else
    $display("PASSED");
end

endmodule

// Test unpacked array variables inside a function.
module array21;

function [3:0] get_first(input [7:0] value);
  reg [3:0] tmp[1:2];
  begin
    tmp[1] = value[7:4];
    tmp[2] = value[3:0];
    get_first = tmp[1];
  end
endfunction

function [3:0] get_second(input [7:0] value);
  reg [3:0] tmp[1:2];
  begin
    tmp[1] = value[7:4];
    tmp[2] = value[3:0];
    get_second = tmp[2];
  end
endfunction

reg failed = 0;
reg [3:0] first;
reg [3:0] second;

initial begin
  first = get_first(8'ha5);
  second = get_second(8'ha5);

  if (first !== 4'ha)
    failed = 1;

  if (second !== 4'h5)
    failed = 1;

  if (failed)
    $display("FAILED");
  else
    $display("PASSED");
end

endmodule

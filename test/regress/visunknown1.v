// $isunknown is true when any bit is X or Z
module visunknown1;
  reg  [3:0]  a = 4'b01x0, b = 4'b0110, c = 4'bz000;
  reg  [69:0] w = 70'h1;
  integer     i = 5;
  wire        f;
  reg  [1:0]  pair;

  assign f = $isunknown(a) || $isunknown(i);

  initial begin
    #1;
    if ($isunknown(a) !== 1'b1) $display("FAILED a");
    if ($isunknown(b) !== 1'b0) $display("FAILED b");
    if ($isunknown(c) !== 1'b1) $display("FAILED c");
    if ($isunknown(w)) $display("FAILED w");
    w[69] = 1'bx;
    if (!$isunknown(w)) $display("FAILED w[69]");
    if ($isunknown(i)) $display("FAILED i");
    if (f !== 1'b1) $display("FAILED f=%b", f);
    pair = {$isunknown(b), $isunknown(a)};   // One bit each
    if (pair !== 2'b01) $display("FAILED pair=%b", pair);
    a = 4'b0000;
    #1 if (f !== 1'b0) $display("FAILED f=%b after a changed", f);
    $display("PASSED");
  end
endmodule

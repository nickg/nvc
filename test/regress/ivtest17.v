module ivtest17;

wire [7:0] val[3:0];

genvar i;

for (i = 3; i >= 0; i = i - 1) begin
  assign val[i] = i;
end

integer j;

reg failed = 0;

initial begin
  #1;  // TODO: is this required to avoid race?
  for (j = 3; j >= 0; j = j - 1) begin
    $display("%d | %x", j, val[j]);
    if (val[j] !== j) failed = 1;
  end
  if (failed)
    $display("FAILED");
  else
    $display("PASSED");
end

endmodule

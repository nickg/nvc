// Sensitivity of an always @(*) block containing a procedural assign and
// a real constant
module sens_assign1;
  integer fail = 0;
  reg [3:0] src;
  reg [3:0] r;
  real scaled;
  always @(*) begin
    assign r = src + 1;
  end
  always @(*) begin
    scaled = src * 2.5;
  end
  initial begin
    #1;
    src = 4;
    #1;
    if (r !== 5) fail = 1;
    if (scaled != 10.0) fail = 1;
    src = 6;
    #1;
    if (r !== 7) fail = 1;
    if (scaled != 15.0) fail = 1;
    if (fail)
      $display("FAILED");
    else
      $display("PASSED");
  end
endmodule

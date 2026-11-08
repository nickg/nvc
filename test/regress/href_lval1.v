module href_lval1;
  integer fail = 0;
  reg [3:0] src;
  sub u();
  assign u.w = src;
  always @(*) begin
    assign u.r = src + 1;
  end
  initial begin
    #1;
    src = 4;
    #1;
    if (u.w !== 4) fail = 1;
    if (u.r !== 5) fail = 1;
    src = 6;
    #1;
    if (u.w !== 6) fail = 1;
    if (u.r !== 7) fail = 1;
    force u.r = 9;
    #1;
    if (u.r !== 9) fail = 1;
    release u.r;
    if (fail)
      $display("FAILED");
    else
      $display("PASSED");
  end
endmodule

module sub;
  wire [3:0] w;
  reg [3:0] r;
endmodule

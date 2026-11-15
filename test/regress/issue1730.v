module issue1730;
  reg [64:0] a65;
  reg [127:0] a128;
  reg failed;

  initial begin
    failed = 0;

    a65 = 65'b1;
    a128 = 128'b1;
    #1;
    if (^a65 !== 1'b1 || ~^a65 !== 1'b0
        || ^a128 !== 1'b1 || ~^a128 !== 1'b0)
      failed = 1;

    a65 = {65{1'b1}};
    a128 = {128{1'b1}};
    #1;
    if (^a65 !== 1'b1 || ~^a65 !== 1'b0
        || ^a128 !== 1'b0 || ~^a128 !== 1'b1)
      failed = 1;

    a65 = {1'b1, 64'b0};
    a128 = {1'b1, 126'b0, 1'b1};
    #1;
    if (^a65 !== 1'b1 || ~^a65 !== 1'b0
        || ^a128 !== 1'b0 || ~^a128 !== 1'b1)
      failed = 1;

    a65 = {1'bx, 64'b0};
    a128 = {1'bz, 127'b0};
    #1;
    if (^a65 !== 1'bx || ~^a65 !== 1'bx
        || ^a128 !== 1'bx || ~^a128 !== 1'bx)
      failed = 1;

    $display("%s", failed ? "FAILED" : "PASSED");
  end
endmodule

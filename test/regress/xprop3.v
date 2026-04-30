module xprop3;
  reg [7:0] lhs, rhs;
  reg [7:0] lt_r, le_r, gt_r, ge_r;
  reg failed;

  initial begin
    failed = 1'b0;

    lhs = 8'h12;
    rhs = 8'h34;
    lt_r = lhs < rhs;
    le_r = lhs <= rhs;
    gt_r = lhs > rhs;
    ge_r = lhs >= rhs;
    $display("known_lt=%b known_le=%b known_gt=%b known_ge=%b",
             lt_r, le_r, gt_r, ge_r);
    if (lt_r !== 8'b00000001 || le_r !== 8'b00000001
        || gt_r !== 8'b00000000 || ge_r !== 8'b00000000)
      failed = 1'b1;

    lhs = 8'h34;
    rhs = 8'h34;
    lt_r = lhs < rhs;
    le_r = lhs <= rhs;
    gt_r = lhs > rhs;
    ge_r = lhs >= rhs;
    $display("equal_lt=%b equal_le=%b equal_gt=%b equal_ge=%b",
             lt_r, le_r, gt_r, ge_r);
    if (lt_r !== 8'b00000000 || le_r !== 8'b00000001
        || gt_r !== 8'b00000000 || ge_r !== 8'b00000001)
      failed = 1'b1;

    lhs = 8'b0001x010;
    rhs = 8'h34;
    lt_r = lhs < rhs;
    le_r = lhs <= rhs;
    gt_r = lhs > rhs;
    ge_r = lhs >= rhs;
    $display("lhs_x_lt=%b lhs_x_le=%b lhs_x_gt=%b lhs_x_ge=%b",
             lt_r, le_r, gt_r, ge_r);
    if (lt_r !== 8'b0000000x || le_r !== 8'b0000000x
        || gt_r !== 8'b0000000x || ge_r !== 8'b0000000x)
      failed = 1'b1;

    lhs = 8'h12;
    rhs = 8'b0011x100;
    lt_r = lhs < rhs;
    le_r = lhs <= rhs;
    gt_r = lhs > rhs;
    ge_r = lhs >= rhs;
    $display("rhs_x_lt=%b rhs_x_le=%b rhs_x_gt=%b rhs_x_ge=%b",
             lt_r, le_r, gt_r, ge_r);
    if (lt_r !== 8'b0000000x || le_r !== 8'b0000000x
        || gt_r !== 8'b0000000x || ge_r !== 8'b0000000x)
      failed = 1'b1;

    lhs = 8'b0001x010;
    rhs = 8'b0011x100;
    lt_r = lhs < rhs;
    le_r = lhs <= rhs;
    gt_r = lhs > rhs;
    ge_r = lhs >= rhs;
    $display("both_x_lt=%b both_x_le=%b both_x_gt=%b both_x_ge=%b",
             lt_r, le_r, gt_r, ge_r);
    if (lt_r !== 8'b0000000x || le_r !== 8'b0000000x
        || gt_r !== 8'b0000000x || ge_r !== 8'b0000000x)
      failed = 1'b1;

    if (failed)
      $display("FAILED");
    else
      $display("PASSED");
  end
endmodule // xprop3

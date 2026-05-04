module xprop4;
  localparam [0:0] const_eq_known_diff = (4'b1x00 == 4'b0x00);
  localparam [0:0] const_ne_known_diff = (4'b1x00 != 4'b0x00);
  localparam [0:0] const_eq_unknown = (4'b10x1 == 4'b10z1);
  localparam [0:0] const_ne_unknown = (4'b10x1 != 4'b10z1);
  localparam [0:0] const_eq_known = (4'b1010 == 4'b1010);
  localparam [0:0] const_ne_known = (4'b1010 != 4'b1010);

  reg [7:0] lhs, rhs;
  reg [70:0] wide_lhs, wide_rhs;
  reg [7:0] eq_r, ne_r;
  reg failed;

  initial begin
    failed = 1'b0;

    eq_r = const_eq_known_diff;
    ne_r = const_ne_known_diff;
    $display("const_known_diff_eq=%b const_known_diff_ne=%b", eq_r, ne_r);
    if (eq_r !== 8'b00000000 || ne_r !== 8'b00000001)
      failed = 1'b1;

    eq_r = const_eq_unknown;
    ne_r = const_ne_unknown;
    $display("const_unknown_eq=%b const_unknown_ne=%b", eq_r, ne_r);
    if (eq_r !== 8'b0000000x || ne_r !== 8'b0000000x)
      failed = 1'b1;

    eq_r = const_eq_known;
    ne_r = const_ne_known;
    $display("const_known_eq=%b const_known_ne=%b", eq_r, ne_r);
    if (eq_r !== 8'b00000001 || ne_r !== 8'b00000000)
      failed = 1'b1;

    lhs = 8'b1x001100;
    rhs = 8'b0x001100;
    eq_r = lhs == rhs;
    ne_r = lhs != rhs;
    $display("narrow_known_diff_eq=%b narrow_known_diff_ne=%b", eq_r, ne_r);
    if (eq_r !== 8'b00000000 || ne_r !== 8'b00000001)
      failed = 1'b1;

    lhs = 8'b1010x100;
    rhs = 8'b1010z100;
    eq_r = lhs == rhs;
    ne_r = lhs != rhs;
    $display("narrow_unknown_eq=%b narrow_unknown_ne=%b", eq_r, ne_r);
    if (eq_r !== 8'b0000000x || ne_r !== 8'b0000000x)
      failed = 1'b1;

    lhs = 8'b10100100;
    rhs = 8'b10100100;
    eq_r = lhs == rhs;
    ne_r = lhs != rhs;
    $display("narrow_known_eq=%b narrow_known_ne=%b", eq_r, ne_r);
    if (eq_r !== 8'b00000001 || ne_r !== 8'b00000000)
      failed = 1'b1;

    wide_lhs = {1'b1, 62'h123456789abcdef, 8'bx0011001};
    wide_rhs = {1'b0, 62'h123456789abcdef, 8'bz0011001};
    eq_r = wide_lhs == wide_rhs;
    ne_r = wide_lhs != wide_rhs;
    $display("wide_known_diff_eq=%b wide_known_diff_ne=%b", eq_r, ne_r);
    if (eq_r !== 8'b00000000 || ne_r !== 8'b00000001)
      failed = 1'b1;

    wide_lhs = {63'h123456789abcdef0, 8'bx0011001};
    wide_rhs = {63'h123456789abcdef0, 8'bz0011001};
    eq_r = wide_lhs == wide_rhs;
    ne_r = wide_lhs != wide_rhs;
    $display("wide_unknown_eq=%b wide_unknown_ne=%b", eq_r, ne_r);
    if (eq_r !== 8'b0000000x || ne_r !== 8'b0000000x)
      failed = 1'b1;

    wide_lhs = {63'h123456789abcdef0, 8'b10011001};
    wide_rhs = {63'h123456789abcdef0, 8'b10011001};
    eq_r = wide_lhs == wide_rhs;
    ne_r = wide_lhs != wide_rhs;
    $display("wide_known_eq=%b wide_known_ne=%b", eq_r, ne_r);
    if (eq_r !== 8'b00000001 || ne_r !== 8'b00000000)
      failed = 1'b1;

    if (failed)
      $display("FAILED");
    else
      $display("PASSED");
  end
endmodule // xprop4

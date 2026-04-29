module xprop1;
  reg x;
  reg and_r, or_r, not_r;
  reg [3:0] tern_r;
  reg [70:0] wide_tern_r;
  reg [70:0] wide_x;
  reg [70:0] wide_cond_tern_r;
  reg wide_cond_narrow_tern_r;

  initial begin
    x = 1'bx;
    wide_x = {71{1'bx}};

    and_r = x && 1'b1;
    or_r = x || 1'b0;
    not_r = !x;
    tern_r = x ? 4'b1010 : 4'b1001;
    wide_tern_r = x ? {71{1'b1}} : {71{1'b0}};
    wide_cond_tern_r = wide_x ? {71{1'b1}} : {71{1'b0}};
    wide_cond_narrow_tern_r = wide_x ? 1'b1 : 1'b0;

    $display("and=%b or=%b not=%b tern=%b wide_tern=%b wide_cond_tern=%b wide_cond_narrow_tern=%b",
             and_r, or_r, not_r, tern_r, wide_tern_r, wide_cond_tern_r,
             wide_cond_narrow_tern_r);

    if (and_r !== 1'bx || or_r !== 1'bx || not_r !== 1'bx
        || tern_r !== 4'b10xx || wide_tern_r !== {71{1'bx}}
        || wide_cond_tern_r !== {71{1'bx}}
        || wide_cond_narrow_tern_r !== 1'bx)
      $display("FAILED");
    else
      $display("PASSED");
  end
endmodule // xprop1

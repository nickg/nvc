module display2;
  reg x;
  reg [7:0] y;
  reg [11:0] z;
  reg [71:0] wide;

  initial begin
    x = 1'b1;
    y = 8'ha5;
    z = 12'habc;
    wide = 72'h123456789abcdef012;

    $displayb("b:",, x,, y,, z);
    $displayo("o:",, x,, y,, z);
    $displayh("h:",, x,, y,, z);
    $displayb("w:",, wide);
    $displayo("w:",, wide);
    $displayh("w:",, wide);
  end
endmodule // display2

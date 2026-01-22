module display1;
  reg x;
  reg [7:0] y;
  integer   i;
  reg [39:0] wide;

  reg signed [15:0] s1, s2;

  initial begin
    x = 0;
    y = 42;
    i = -1064739199;
    wide = 40'h2389f8924;
    s1 = 16'h2759;
    s2 = 16'h65f1;
    $display("hello",,,x,,y);   // "hello  0  42"
    $display("i=%d", i);        // "i=-1064739199"
    $display("%t | wide=%x - %x  ==> %x", $time, wide, 40'h1, wide - 1);  // "                   0 | wide=02389f8924 - 0000000001  ==> 02389f8923"
    $display("%x - %x ==> %x", s1, s2, s1 - s2);   // "2759 - 65f1 ==> c168"
  end
endmodule // display1

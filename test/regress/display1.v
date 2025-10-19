module display1;
  reg x;
  reg [7:0] y;
  integer   i;

  initial begin
    x = 0;
    y = 42;
    i = -1064739199;
    $display("hello",,,x,,y);   // hello  0  42
    $display("i=%d", i);        // i=-1064739199
  end
endmodule // display1

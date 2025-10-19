module display1;
  reg x;
  reg [7:0] y;

  initial begin
    x = 0;
    y = 42;
    $display("hello",,,x,,y);   // hello  0  42
  end
endmodule // display1

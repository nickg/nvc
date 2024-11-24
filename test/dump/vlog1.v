module dff (input d, clk, rstb,
            output reg q);
  always @(posedge clk)
    q <= d;
endmodule // dff

module mod2;
  wire [7:0] bus;
  wire       w;
  reg        r;
  initial begin
    $display("hello", $time);
    if (bus)
      r <= 1 | r;
    $finish;
    r = 1;
    #1 r <= 0;
    r = ~w;
    r = #1 5;
    r <= #5 1;
    do w = r && w; while (r);
    while (1);
    repeat (5) r = r + 1;
  end
  assign bus = 3;
  pullup (supply1, supply0) p1 (w);
  mod u1 (w);
  assign bus[2] = 4'b1;
endmodule // mod2

primitive multiplexer (mux, control, dataA, dataB);
  output mux;
  input  control, dataA, dataB;
  table
// control dataA dataB   mux
    0        1     ?   :  1 ;   // ? = 0 1 x
    0        0     ?   :  0 ;
    1        ?     1   :  1 ;
    1        ?     0   :  0 ;
  endtable
endprimitive

module mod3;   // Check operator precedence
  wire x, y, z;
  initial begin
    if (x || y === z);
    if (x & y | y & z === x + z);
    if (x == y || y == z);
    if ((x & y) == z);
  end
endmodule // mod3


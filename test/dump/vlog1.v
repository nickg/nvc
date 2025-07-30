module dff (input d, clk, rstb,
            output reg q);
  always @(posedge clk)
    q <= d;
endmodule // dff

module mod2;
  wire [7:0] bus;
  wire       w;
  reg        r;
  integer    i;
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
    for (i = 0; i < 10; i++)
      r = !r;
    #1;
    forever i = ++i;
  end
  assign bus = 3;
  pullup (supply1, supply0) g1 (w);
  mod #(42) u1 (w), u2(bus[1]);
  assign bus[2] = 4'b1;
  parameter [7:0] p1 = 5;
  localparam bit p2 = 0;
  reg [7:0]      array1 [127:0];
  initial
    casex (w)
      1'b0, 1'b1: r = p1[3:0];
      default: r = 0;
    endcase
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
  real v1 = 1.5;
  integer v2 = x ? 1 : 2;
  assign z = {x, y};
  assign zz = {5{z}};
endmodule // mod3

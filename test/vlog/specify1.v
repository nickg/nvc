module test1(a, b, c, d);
  input  a, b, c, d, e, f, clk, rst;
  output r, s, t, u, v, w;
  reg notifier;
  wire delayed_clk, delayed_d;

  specify
    (a => r) = 1;
    (b => s) = (1.2, 5.2);
    (c + => t) = 2;
    (a,b,c -*> u,v,w) = 2.5;
    if (a)
      (posedge d,e => u,v: (5+6)) = (2.3,4.5);
    if (b)
      (negedge e,f -*> r,s+:1) = 7;
    if (c)
      (edge d *> w : 1) = 8;

    $setup(a, clk, 0.5);
    $setup(b, clk, 0.4,);
    $setup(c, clk, 0.4, notifier);
    $hold (clk, a, 0.1);
    $hold (clk, b, 0.2,);
    $hold (clk, c, 0.2, notifier);
    $recovery(rst, d, 0.5);
    $removal(rst, e, 0.2);
    $width(clk, 10, 11);
    $setuphold (posedge clk, negedge a, 0, 0, notifier,,, delayed_clk, delayed_d);
    $recrem (negedge rst, posedge clk, 0, 0, notifier,,,,);
  endspecify

endmodule // test1

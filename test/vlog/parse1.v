module parse1;
  wire [8:0] x, y, z;
  reg        z;
  assign y = x;
  always begin : foo
    $display("hello");
    $finish;
    if (x);
    if (x) z <= 1;
    if (x) $display("yes");
    else if (z);
    else $display("no");
    z = 0;
    z = ~x;
    z = !x;
  end
  assign x = x | y;
  pulldown (supply0) p1 (x);
  pulldown p2 (x);
  pullup p3 (y);
  pullup (supply0, supply1) p4 (y);
  pullup (y);
  always @(x or y or (posedge z)) begin
  end
  initial begin
    while (z);
    repeat (x) x = ~x;
    do x = y; while (1);
  end
  initial begin
    x = x << 1;
    x = x >> 1;
    x = y <<< x;
    x = x >>> y;
  end
  int r1 = 5;
  wire w1 = 0;
endmodule // parse1

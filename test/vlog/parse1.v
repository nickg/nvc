module parse1;
  wire [8:0] x, y;
  reg        z;
  assign y = x;
  always begin : foo
    $display("hello");
    $finish;
    if (x);
    if (x) z <= 1;
  end
endmodule // parse1

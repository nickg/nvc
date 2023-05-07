module parse1;
  wire [8:0] x, y;
  assign y = x;
  always begin : foo
    $display("hello");
    $finish;
  end
endmodule // parse1

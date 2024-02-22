module vlog7;
  wire w1, w2, w3;
  pullup (w1);
  pullup (supply1) (w2);
  pulldown (w3);

  assign w1 = 0;
  assign w2 = 0;

  initial begin
    $display("%x %x %x", w1, w2, w3);   /// XXX: wrong
    #0;
    $display("%x %x %x", w1, w2, w3);
    $finish;
  end
endmodule // vlog7

module gate1;
  wire w;
  reg  r;
  pullup (w); // OK
  pulldown (supply0) p1 (r); // OK
  pullup (supply1) p1 (w); // Error
endmodule // gate1

// Parse errors
module enum1;
  enum { a, b } [3:0] t_x;   // OK
  typedef enum { c, d } t_y;   // OK
  t_y var1;  // OK
  a var2; // Error
  enum byte { e = 6, f } z;    // OK
  enum byte { g = f, h } zz;   // OK
  enum byte { i = zz, j } zzz; // Error
  enum logic [3:0] { A = 4'b0000, B = 4'b0001} x; // OK

  initial begin
    t_y x;
    x = x.first();   // OK
    x = x.last();    // OK
    x = x.next();    // OK
    x = x.prev();    // OK
    x = x.num();     // OK
    x = x.name();    // OK
    x = x.invalid(); // Error
  end
endmodule // enum1

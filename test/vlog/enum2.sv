// Semantic errors
module enum2;
  enum { a, b } [3:0] t_x;   // OK
  typedef enum { c, d } t_y;   // OK
  t_y var1;  // OK
  enum byte { e = 6, f } z;    // OK
  enum byte { g = f, h } zz;   // OK
  enum byte { i = zz, j } zzz; // Error
  enum logic [3:0] { A = 4'b0000, B = 4'b0001} x; // OK

endmodule // enum2

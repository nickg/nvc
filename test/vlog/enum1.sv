module enum1;
  enum { a, b } [3:0] t_x;   // OK
  typedef enum { c, d } t_y;   // OK
  t_y var1;  // OK
  a var2; // Error
  enum byte { e = 6, f } z;    // OK
  enum byte { g = f, h } zz;   // OK
  enum byte { i = zz, j } zzz; // Error
endmodule // enum1

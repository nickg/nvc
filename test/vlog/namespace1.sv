typedef enum { a, b, c } t_abc;

class class1;
  t_abc f;  // OK
endclass

module mod;
  t_abc v1;   // OK
  class1 v2;  // OK
endmodule // mod

typedef int t_useless;

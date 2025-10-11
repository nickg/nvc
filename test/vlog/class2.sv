program main;

  class foo_t;
    int a;
    int b;
  endclass : foo_t

  foo_t obj, copy;   // OK

  class bar_t;
    real x;    // OK
  endclass // bar_t

  int x;   // OK

  bar_t bar;   // OK

  initial begin
    if (obj == null);  // OK
    if (x == null);    // Error
    if (obj.a == 5);   // OK
    obj = obj + copy;  // Error
    obj = bar;         // Error
    obj = 5;           // Error
    if (obj == bar);   // Error
  end

endprogram

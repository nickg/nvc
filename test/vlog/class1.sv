program main;

  class foo_t;
    int a;
    int b;
    ;         // OK
  endclass : foo_t

  foo_t obj, copy;  // OK

  class bar_t ;
  endclass : foo_t   // Error

  int x;

  initial begin
    obj = new;       // OK
    x = new;         // Error
    obj.a = x;       // OK
    obj.c = 5;       // Error
  end

endprogram : bob

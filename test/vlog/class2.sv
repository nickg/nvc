program main;

  class foo_t;
    int a;
    int b;
  endclass : foo_t

  foo_t obj, copy;   // OK

  int x;

  initial begin
    if (obj == null); // OK
    if (x == null);   // Error
    if (obj.a == 5);  // OK
  end

endprogram

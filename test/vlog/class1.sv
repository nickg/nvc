program main;

   class foo_t;
      int a;
      int b;
   endclass : foo_t

   foo_t obj, copy;   // OK

  class bar_t ;
  endclass : foo_t   // Error

  int x;

  initial begin
    if (obj == null)  // OK
      obj = new;      // OK

    x = new;          // Error
    if (x == null);   // Error
  end

endprogram : bob

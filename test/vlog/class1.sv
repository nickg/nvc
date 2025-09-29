program main;

   class foo_t;
      int a;
      int b;
   endclass : foo_t

   foo_t obj, copy;   // OK

  class bar_t ;
  endclass : foo_t   // Error


endprogram : bob

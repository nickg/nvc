primitive multiplexer (mux, control, dataA, dataB);
  output mux;
  input  control, dataA, dataB;
  table
// control dataA dataB   mux
    0        1     ?   :  1 ;   // ? = 0 1 x
    0        0     ?   :  0 ;
    1        ?     1   :  1 ;
    1        ?     0   :  0 ;
  endtable
endprimitive

primitive test1 (x, y, z /* Error */);
  input  x;   // Error
  output y;   // Error
  table
    0 0 : 1 ;
  endtable
endprimitive

primitive test2 (x, y, z);
  output x;
  input  y, z;
  table
    0 : 1 ;     // Error
    0 0 0 : 0;  // Error
  endtable
endprimitive

primitive dff(q, cp, d);
   output q;
   input  cp, d;
   reg	  q;

   table
   // (cp)  d  :  q  :  q  ;
        ?   *  :  ?  :  -  ;
      (?0)  ?  :  ?  :  -  ;
      (1x)  ?  :  ?  :  -  ;
      (x1)  0  :  0  :  0  ;
      (x1)  1  :  1  :  1  ;
      (0x)  0  :  0  :  0  ;
      (0x)  1  :  1  :  1  ;
      (01)  0  :  ?  :  0  ;
      (01)  1  :  ?  :  1  ;
   endtable

endprimitive

primitive test3 (x, y, z);
  output reg x;
  input      y, z;
  table
    r ? : 0 : 1 ;
    r ? : 1 : 0 ;
    r (01) : 1 : 0 ; // Error
  endtable
endprimitive

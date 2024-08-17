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

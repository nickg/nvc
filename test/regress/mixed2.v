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

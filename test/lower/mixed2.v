
primitive UDP_MUX2 (Q, A, B, SL);
  output Q;
  input A, B, SL;
  table
    //  A   B   SL  :   Q
    0   0   ?   :   0 ;
    1   1   ?   :   1 ;
    0   ?   0   :   0 ;
    1   ?   0   :   1 ;
    ?   0   1   :   0 ;
    ?   1   1   :   1 ;
    x   ?   0   :   x ;
    ?   x   1   :   x ;
    1   0   x   :   x ;
    0   1   x   :   x ;
  endtable
endprimitive

module SLE_Prim (output Q,
                 input ADn,
                 input ALn,
                 input CLK,
                 input D,
                 input LAT,
                 input SD,
                 input EN,
                 input SLn);
  
  UDP_MUX2 mux_0(SYNC, SD, D, SLn);
  UDP_MUX2 mux_1(DATA, Q, SYNC, EN);
endmodule

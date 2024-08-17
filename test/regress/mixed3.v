primitive dff(q, cp, d);
   output q;
   input  cp, d;
   reg	  q;

   initial
     q = 1'b1;
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

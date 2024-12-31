`define MACRO1 \
               \
               \
               \
  module       \
  123

`define MACRO2 `MACRO1
    
module pp4;
  wire x = `MACRO2;    // Error

  wire a = 1;
  wire b = 2;
  wire c = 3;
  wire wire wire;   // Error
  
endmodule // pp4

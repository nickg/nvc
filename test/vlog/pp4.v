`define MACRO1 \
               \
               \
               \
  module       \
  123

`define MACRO2 `MACRO1

`define NUMBER 4'b1010

module pp4;
  wire w1 = (5 == `NUMBER);  // OK
  wire w2 = ((5 == `NUMBER));  // OK

  wire x = `MACRO2;    // Error

  wire a = 1;
  wire b = 2;
  wire c = 3;
  wire wire wire;   // Error

endmodule // pp4

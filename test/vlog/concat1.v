module concat1;
  wire       x, y;
  wire [7:0] z;
  real       r1;
  reg        q;
  
  assign a1 = {x, y};   // OK
  assign a2 = {r1, x};  // Error
  assign {z[0], z[4]} = {x, y};   // OK
  assign {x, q} = 2'b11;  // Error
  assign a3 = {5{x}};   // OK
  
endmodule // concat1

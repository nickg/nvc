module ports1 (x, y, z, y);  // Error
  input      x;
  output reg y;
  output     y;  // Error
  output     z;
  reg        z;  // OK
endmodule

module ports2 (x, y, z);
  input x;
  always @(x) y <= x; // Error (iverilog accepts it?)
  output reg y;
  inout      z;   // OK
endmodule // ports2

module ports3 (x, y, z);
  reg    x;
  output x;
  output x;   // Error
  input  y;
  reg    y;
  reg    y;   // Error
  input  z;
  wire   z;
endmodule // ports3

module ports4 (input i0, i1, output reg o1, o2, output o3);
  initial begin
    o1 = 1;
    o2 = 0;
    o3 = 1;    // Error (sem)
  end
endmodule // ports4

module ports5 (x, y, z);
  input x, y;    // OK
  output reg z;
endmodule // ports5

module ports9;
  output o;  // Error
endmodule // ports9

module ports10 (input wire i0);
  input i1;  // Error
endmodule // ports10

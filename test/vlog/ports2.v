module ports4 (input i0, i1, output reg o1, o2, output o3);
  initial begin
    o1 = 1;
    o2 = 0;
    o3 = 1;    // Error
  end
endmodule // ports4

module ports5 (x, y, z);
  input x, y;    // OK
  output reg z;
endmodule // ports5

module ports6 (x, y, z);
  input [7:0]  x;    // OK
  output [3:0] y;    // OK
  wire [5:0]   y;    // Error
  input [x:0]  z;    // Error
endmodule // ports6

module ports7 (x, y, z, zz /* Error */);
  input x, y, z;     // OK
endmodule // ports7

module ports8 (input wire i0);
endmodule // ports8

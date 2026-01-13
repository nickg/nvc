module sub1(x, y);
  parameter p;
  input x;
  output y;
endmodule // sub1

module vlog1;
  wire x, y;
  reg  z;

  sub1 #(1) u1 (x, y);    // OK
  SUB1 #(2) u2 (x, y);    // Error
  sub1 #(3) u3 (x);       // Error
  bad #(4) u4 (x);        // Error
  sub1 u5 (x, y);         // Error
  sub1 #(1, 2) u6 (x, y); // Error
  sub1 #(1) u7 (.foo(x), .y(y)); // Error
  sub1 #(1) u8 (1, z);    // Error
  sub1 #(1) u9 (1, 2);    // Error
  sub1 #(1) u10 (.x(), .y()); // OK
endmodule // vlog1

module sub1(x, y);
  input x;
  output y;
endmodule // sub1

module vlog1;
  wire x, y;
  
  sub1 u1 (x, y);    // OK
  SUB1 u2 (x, y);    // Error
  sub1 u3 (x);       // Error
  bad u4 (x);        // Error
endmodule // vlog1

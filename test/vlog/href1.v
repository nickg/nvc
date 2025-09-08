module href1;

  assign w1 = u1.x;   // OK
  assign w2 = w3.x;   // Error
  assign w3 = xx.x;   // Error

  sub u1();

  assign w4 = u1.x;   // OK

endmodule // href1

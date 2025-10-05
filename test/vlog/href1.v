// Parse errors
module href1;

  assign w1 = u1.x;   // OK
  assign w2 = w3.x;   // Error
  assign w3 = xx.x;   // Error

  sub u1();

  assign w4 = u1.x;   // OK

  defparam u1.p = 6;  // OK
  defparam yy.x = 7;  // Error
  defparam u1.p.q = 8; // OK

endmodule // href1

// Semantic errors
module href1;

  assign w1 = u1.x;   // OK
  assign w2 = w3.x;   // Error
  assign w3 = 1'b1;

  sub u1();

  assign w3 = u1.x;   // OK

endmodule // href1

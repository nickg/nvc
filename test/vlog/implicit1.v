module implicit1;
  assign x1 = 1'b1;   // OK
  and u1(x2, x1);     // OK
  or u2(x1, x3);      // OK
  mymod u3(x3, x4);   // OK
  pulldown (x5);      // OK
endmodule   // implicit1

module real1;
  real r1;
  time t1;

  initial begin
    if (r1 > 1.0);       // OK
    t1 = t1 * 0.5;       // OK
    r1 = r1 % 2;         // Error
    r1 = r1 & 5;         // Error
    r1 = r1 ^ r1;        // Error
    if (r1 === 0.0);     // Error
    r1 = r1 > 2.0 ? r1 : 0;   // OK
    r1[5] = 0;           // Error
    r1 = ~r1;            // Error
    r1 = +r1;            // OK
  end

endmodule // real1

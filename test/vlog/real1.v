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
  end // initial begin

  initial begin
    r1 = 1.2;
    r1 = 0.1;
    r1 = 2394.26331;
    r1 = 1.2E12;           // the exponent symbol can be e or E
    r1 = 1.30e-2;
    r1 = 0.1e-0;
    r1 = 23E10;
    r1 = 29E-2;
    r1 = 236.123_763_e-12; // underscores are ignored
  end // initial begin

endmodule // real1

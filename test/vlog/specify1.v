module test1(a, b, c, d);
  input a, b;
  output c, d;

  specify
    (a => c) = 1;
    (b => d) = (1.2, 5.2);
  endspecify
  
endmodule // test1

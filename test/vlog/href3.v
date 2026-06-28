module sub1();
endmodule // sub1

module href3();
  sub1 u1();

  assign w1 = u1.xxx;  // Error
  assign w2 = glbl.x;  // Error

endmodule // href3

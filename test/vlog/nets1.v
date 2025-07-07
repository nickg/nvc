module nets1;

  wand #2 n1;
  wor #(2,8,1) n2;
  trior n3;
  triand n4;
  trireg (medium) n5;

  tri0 n6;
  tri1 (small) n7; //Error

  tri0 vectored [3:0] #(1, 2, 4) n8;
  tri0 scalared #1 n9; // Error

  interconnect #3 n10 [2:1];

  tri0 (weak0, strong1) #(2, 3, 5) n11;

  tri1 (pull0, highz1) n12;

  reg r1;
  reg r2;
  reg r3;

  assign (highz1, strong0)           n2 = r1;
  assign (weak0,  pull1)  #5         n4 = r2;
  assign (highz0, highz1) #(3, 2, 1) n5 = r3; // Error

endmodule // nets1

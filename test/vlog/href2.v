module glbl();
    wire GSR;
endmodule // glbl

module href2();
  assign o = ~glbl.GSR;  // OK
endmodule // href2

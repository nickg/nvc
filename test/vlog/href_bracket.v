// Mid-path constant_bit_select in hierarchical identifiers
module href_bracket;

  sub inst[3:0] ();

  assign x = inst[2].sig;       // OK
  assign y = gen[0].sub.x;      // OK
  assign z = a[1].b[2].c;       // OK
  assign w = inst[].sig;        // Error

endmodule // href_bracket

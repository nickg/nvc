`default_nettype wire     // OK
`default_nettype tri      // OK
`default_nettype tri0     // OK
`default_nettype tri1     // OK
`default_nettype wand     // OK
`default_nettype triand   // OK
`default_nettype wor      // OK
`default_nettype trior    // OK
`default_nettype trireg   // OK
`default_nettype uwire    // OK
`default_nettype foo      // Error
`default_nettype none     // OK

module foo;
  assign x = 5;   // Error
endmodule // foo

`resetall                 // OK

`unconnected_drive pull1 // OK

module bar;
  wire y;
  assign x = 5;   // OK
endmodule // foo

`nounconnected_drive // OK

`unconnected_drive wire // Error

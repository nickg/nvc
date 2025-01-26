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
`default_nettype none     // OK
`default_nettype foo      // Error

module foo;
endmodule // foo

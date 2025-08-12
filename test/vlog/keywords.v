`begin_keywords "1364-1995"
module bar;
  reg logic = 1; // OK
  wire do = 5; // OK
endmodule // foo
`end_keywords

module foo;
  reg logic = 1; // Warning
  wire do = 5; // Suppressed
endmodule // foo

`end_keywords  // Error
`begin_keywords "fizz"  // Error
`begin_keywords 123

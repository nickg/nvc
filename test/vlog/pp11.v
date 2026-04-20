`define MACRO1(a=5,b="B",c) $display(a,,b,,c);
`MACRO1 ( , 2, 3 )
`MACRO1 ( 1 , , 3 )
`MACRO1 ( , 2, )

`define MACRO2(a=5, b, c="C") $display(a,,b,,c);
`MACRO2 (1, , 3)
`MACRO2 (, 2, )
`MACRO2 (, 2)

`define MACRO3(a=5, b=0, c="C") $display(a,,b,,c);
`MACRO3 ( 1 )
`MACRO3 ( )

`MACRO1 ( 1 )       // ILLEGAL: b and c omitted, no default for c
`MACRO2 ( , , 3, 4) // ILLEGAL: more argument than declared
`MACRO3             // ILLEGAL: parentheses required

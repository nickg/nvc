`define add(x, y) ((x) + (y))
`add(1, 2)
hello
`define ASSERT assert
`ASSERT(x == y);
`add((1 + 2), ident)
`add(1) // Error
`baz(4, 5, 6) // Warning
`define foo(fff)
`foo(4, 5, 6) // Error
`define pow2(a) a * a
`pow2('hAbCdEf10)         // hex literal (no size)
`pow2(12'b0100_0000_0000) // unsigned binary literal (with underscores)
`pow2(12'd1024)           // unsigned decimal literal
`pow2(12'sh400)           // signed hex literal
`pow2(12'o2000)           // unsigned octal literal

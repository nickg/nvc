`define WIDEN(vec, fromW, toW) \
    /* verilator lint_off WIDTH */ \
    ((toW) == (fromW) ? vec : {{((toW) - (fromW)){1'b0}}, vec}) \
    /* verilator lint_on WIDTH */
`define COMMENTED(x) x /* a block comment */ + 1
`define MULTILINE(x) x /* a block
comment */ + 2

`ifdef NOT_DEFINED
`define HIDDEN /* comment in macro */ 1
`undef COMMENTED
`undefineall
`include not-a-string
`endif

module pp18;
  wire [3:0] i;
  wire [7:0] o = `WIDEN(i, 4, 8);
  wire [3:0] p = `COMMENTED(1);
  wire [3:0] q = `MULTILINE(1);
endmodule

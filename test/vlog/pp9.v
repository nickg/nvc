// Test for macro expansion inside tokens (e.g. 32'd`MACRO)
`define MY_WIDTH 8
`define MY_VAL 42

module pp9;
  parameter P = 32'd`MY_VAL;                       // OK: 32'd42
  localparam W = `MY_WIDTH;                         // OK: 8
  wire [`MY_WIDTH-1:0] bus;                         // OK: [7:0]
endmodule

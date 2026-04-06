`else   // Error
1 2 3 4 5  // Resync parser
`endif  // Error
1 2 3 4 5  // Resync parser
`elsif  // Error
1 2 3 4 5  // Resync parser
`ifdef  // Error
1 2 3 4 5  // Resync parser
`ifndef // Error
1 2 3 4 5  // Resync parser
`ifdef FOO
1 2 3 4 5  // Resync parser
`elsif  // Error
1 2 3 4 5  // Resync parser
`define 1 // Error
1 2 3 4 5  // Resync parser
`ifndef FOO
1 2 3 4 5  // Resync parser
`elsif FOO
1 2 3 4 5  // Resync parser
// Error

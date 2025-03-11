`define FOO bar
`define BAZ 1
 // comment
`FOO = `BAZ
`ifdef FOO   // Another comment
pass
`else
fail
`endif
`ifndef BAZ
fail
`else
pass
`endif
`__FILE__
`__LINE__


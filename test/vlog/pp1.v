`define FOO bar
`define BAZ 1
 // comment
`FOO = `BAZ
`ifdef FOO   // Another comment
pass
`else
fail: FOO is not defined
`endif
`ifndef BAZ
fail: BAZ is defined
`else
pass
`endif
`__FILE__
`__LINE__
`undefineall
`ifdef FOO
fail: FOO is still defined
`elsif BAZ
fail: BAZ is defined
`endif

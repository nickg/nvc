`define TEST
`ifdef	VERIFIC
`define	FORMAL_VERIFIC
`ifdef	SKIDBUFFER
	assume	property (IDATA_HELD_WHEN_NOT_READY);
`elsif TEST
error
`else
	assert	property (IDATA_HELD_WHEN_NOT_READY);
`endif
`endif //  `ifdef VERIFIC

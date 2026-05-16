`define assert(expr) empty_statement

module pp11;
  reg mem_do_wdata;

  initial begin
    if (mem_do_wdata)
      `assert(!mem_do_wdata);
  end

`ifndef USE_REGS
`else
  `UNDEFINED_REGS regs ();
`endif

  task empty_statement;
    begin end
  endtask
endmodule

module vlog18;
  reg x = 1, y;
  reg [7:0] z = 42;

  initial begin
    // This is not a race in System Verilog
    if (x !== 1 || y !== 1'bx || z !== 42)
      $display("FAILED -- %x %x %x", x, y, z);
    else
      $display("PASSED");
  end

endmodule // vlog18

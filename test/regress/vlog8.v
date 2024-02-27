module vlog8();
  reg [3:0] a, b;

  initial begin
    a = 1;
    b = 2;
    #1;
    a = a + b;
    b = a + b;
    if (a !== 3)
      begin
        $display("FAILED -- a: %x !== 3", a);
        $finish;
      end
    if (b !== 5)
      begin
        $display("FAILED -- b: %x !== 5", b);
        $finish;
      end
    #2;
    $display("PASSED");
  end // initial begin

  initial begin
    #2;
    if (a !== 3)
      begin
        $display("FAILED -- a (signal): %x !== 3", a);
        $finish;
      end
    if (b !== 5)
      begin
        $display("FAILED -- b (signal): %x !== 5", b);
        $finish;
      end
  end

endmodule // testbench

module vlog2;

  initial begin
    $display("hello, world!");
    $write("string=%s\n", "foo");
    //$display("%s %d %x %d", "foo", 42, 16, 0);
    $finish;
  end

endmodule // vlog2

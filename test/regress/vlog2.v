// This file has DOS line endings
module vlog2;

  `define MSG "hello, world!"

  initial begin
    $display(`MSG);
    $write("string=%s\n", "foo");
    $display("%s %d %x %d", "foo", 42, 5, 0);
    $finish;
  end

endmodule // vlog2

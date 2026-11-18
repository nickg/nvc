module vemptyarg1;
  integer i = 5;
  initial begin
    $display("a", , "b");
    $write("x=%0d", i, );
    $display("|");
    $write("y=%0d", i + 1, , );
    $display("|");
  end
endmodule

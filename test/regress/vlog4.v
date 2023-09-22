module vlog4;
  reg [7:0] x;

  initial begin
    x <= 42;
    $display("x ==> %d", x);
    #1 $display("x ==> %d", x);
  end
endmodule // vlog4

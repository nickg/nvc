module param1;
  wire [7:0] o1, o2;

  sub #(42) u1(o1);
  sub #(55) u2(o2);

  initial begin
    #1;
    $display("%d %d", o1, o2);
    if (o1 === 42 && o2 == 55)
      $display("PASSED");
    else
      $display("FAILED");
    $finish;
  end
endmodule // param1

module sub(o);
  output [7:0] o;
  parameter    x = 5;
  assign o = x;
endmodule // sub

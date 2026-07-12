module href2;
  reg fail = 0;
  initial begin
    #0;
    if (u1.u2.x !== 5) fail = 1;
    #1;
    #0;
    if (u1.u2.x !== 7) fail = 1;
    #2;
    #0;
    if (u1.u2.x !== 42) fail = 1;
    if (u1.y !== 77) fail = 1;
    if (u1.u2.y !== 77) fail = 1;
    if (fail)
      $display("FAILED");
    else
      $display("PASSED");
  end // initial begin

  always @(*)
    $display("%t | u1.u2.x ==> %d\tu1.y ==> %d\tu1.u2.y ==> %d",
             $time, u1.u2.x, u1.y, u1.u2.y);

  sub1 u1();

endmodule // href2

module sub1;
  wire [7:0] y;
  sub2 u2(.y(y));
endmodule // sub1

module sub2(x, y);
  output reg [7:0] x;
  output     [7:0] y;

  assign y = 77;

  initial begin
    x = 5;
    #1;
    x = 7;
    #2;
    x = 42;
  end
endmodule // sub2

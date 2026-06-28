module href1;
  reg fail = 0;
  initial begin
    #0;
    if (u.x !== 5) fail = 1;
    #1;
    #0;
    if (u.x !== 7) fail = 1;
    #2;
    #0;
    if (u.x !== 42) fail = 1;
    if (fail)
      $display("FAILED");
    else
      $display("PASSED");
  end // initial begin

  always @(u.x)
    $display("%t | sub.x ==> %d", $time, u.x);

  sub u();

endmodule // href1

module sub;
  reg [7:0] x;

  initial begin
    x = 5;
    #1;
    x = 7;
    #2;
    x = 42;
  end
endmodule // sub

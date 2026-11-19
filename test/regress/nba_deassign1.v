// A variable released by deassign must see a non-blocking assignment at the
// time step it is made, not at the next scheduled event
module nba_deassign1;
  reg clk = 0;
  reg d = 0;
  reg q;
  integer rose = 0;
  integer fail = 0;

  always #5 clk = ~clk;

  initial begin
    assign q = 1'b0;
    #20;
    deassign q;
  end

  always @(posedge clk) q <= d;

  always @(q)
    if (q === 1'b1 && rose == 0)
      rose = $time;

  initial begin
    #26;
    d = 1;
    #20;                        // Rising edge at 35, falling edge at 40
    if (rose !== 35) fail = 1;
    if (q !== 1'b1) fail = 1;
    if (fail)
      $display("FAILED");
    else
      $display("PASSED");
    $finish;
  end
endmodule

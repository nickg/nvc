`timescale 10us/1ns
module vtimeformat1_sub;
  initial begin
    #5;
    $display("sub: [%t] [%0t]", $time, $time);
    $timeformat(-6, 1, " us", 0);
    $display("sub: [%t]", $time);
  end
endmodule

`timescale 1ns/100ps
module vtimeformat1;
  vtimeformat1_sub u1();
  real r;
  initial begin
    #51;
    // The default unit is the simulation precision of 100 ps
    $display("[%t] [%0t] [%6t]", $time, $time, $time);
    $timeformat(-9, 0, " ns", 10);
    $display("[%t] [%0t]", $time, $time);
    $timeformat(-9, 2, "ns", 0);
    $display("[%t] [%12t]", $time, $time);
    $timeformat(-12, 0, "", 0);
    $display("[%t]", $time);
    $timeformat(-6, 3, " us", 0);
    $display("[%t]", $time);
    $timeformat(-6, 1, " us", 0);
    $display("[%t]", $time);
    $timeformat(-6, 0, "", 0);
    $display("[%t] [%t]", 64'd499, 64'd500);
    $timeformat(-6, 2, "", 0);
    $display("[%t]", 64'd999);
    #0.5;
    r = $realtime;
    $timeformat(-9, 1, "", 0);
    $display("[%t] [%t]", $realtime, r);
    $timeformat(-10, 0, "", 0);
    $write("[%t]", $realtime);
    $write("\n");
    $timeformat;
    $display("[%t]", $time);
    #1000;
    $display("[%t]", $time);
  end
endmodule

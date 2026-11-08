`timescale 1ms/1ms
module vtimescale2_sub;
  initial begin
    #1500;
    $display("sub: %0d", $time);
    $timeformat(-3, 0, " ms", 0);
    $display("sub: [%0t]", $time);
  end
endmodule

`timescale 1s/1s
module vtimescale2;
  vtimescale2_sub u1();
  initial begin
    #1;
    $display("top: %0d", $time);
    #1s;
    $timeformat(-3, 0, " ms", 0);
    $display("top: %0d [%0t] %0t", $time, $time, $realtime);
  end
endmodule

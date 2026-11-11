`timescale 10ns/1ns
module vtimescale1_b;
  integer fail = 0;
  // $time rounds to the nearest unit of the module, ties round up
  initial #1.5 begin
    if ($time !== 2) fail = 1;
    if ($realtime != 1.5) fail = 1;
  end
  initial #1.6 begin
    if ($time !== 2) fail = 1;
    if ($realtime != 1.6) fail = 1;
  end
  initial #2.4 begin
    if ($time !== 2) fail = 1;
    if ($realtime != 2.4) fail = 1;
  end
  initial #4.1 begin
    if ($time !== 4) fail = 1;
    if ($realtime != 4.1) fail = 1;
  end
  initial #5.6 begin
    if ($time !== 6) fail = 1;
    if ($realtime != 5.6) fail = 1;
  end
endmodule

`timescale 1ns/1ps
module vtimescale1;
  parameter P = 3;
  integer fail = 0;
  vtimescale1_b u1();
  initial begin
    #(P);
    if ($time !== 3) fail = 1;
    #P;
    if ($time !== 6) fail = 1;
    #(1 + 1);
    if ($time !== 8) fail = 1;
    #(10);
    if ($time !== 18) fail = 1;
    #0.5;
    if ($realtime != 18.5) fail = 1;
    if ($time !== 19) fail = 1;         // Ties round up
    #0.4;
    if ($realtime != 18.9) fail = 1;
    if ($time !== 19) fail = 1;
    #0.2;
    if ($realtime != 19.1) fail = 1;
    if ($time !== 19) fail = 1;
  end
  // Reference simulator values: $time is rounded to the nearest unit
  initial #0.4 begin
    if ($time !== 0) fail = 1;
    if ($realtime != 0.4) fail = 1;
  end
  initial #0.5 begin
    if ($time !== 1) fail = 1;
    if ($realtime != 0.5) fail = 1;
  end
  initial #16.4 begin
    if ($time !== 16) fail = 1;
    if ($realtime != 16.4) fail = 1;
  end
  initial #16.5 begin
    if ($time !== 17) fail = 1;
    if ($realtime != 16.5) fail = 1;
  end
  initial #16.6 begin
    if ($time !== 17) fail = 1;
    if ($realtime != 16.6) fail = 1;
  end
  initial #17.5 begin
    if ($time !== 18) fail = 1;
    if ($realtime != 17.5) fail = 1;
  end
  initial begin
    #100;
    if (u1.fail) fail = 1;
    if (fail)
      $display("FAILED");
    else
      $display("PASSED");
  end
endmodule

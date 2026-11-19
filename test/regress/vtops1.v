// Two top-level modules elaborated together: the first refers to an object
// in the second by hierarchical name
module vtops1;
  reg fail = 0;
  initial begin
    #1;
    if (vtops1_b.x !== 5) fail = 1;
    #2;
    if (vtops1_b.x !== 7) fail = 1;
    if (fail)
      $display("FAILED");
    else
      $display("PASSED");
  end
endmodule

module vtops1_b;
  reg [7:0] x;

  initial begin
    x = 5;
    #2;
    x = 7;
  end
endmodule

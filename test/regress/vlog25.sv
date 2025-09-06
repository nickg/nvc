module sub(o);
  output reg [2:0] o;
  initial o = 0;
  always #5 o += 1;
endmodule // sub

module vlog25;
  wire x, y, z;
  reg  failed = 0;

  sub u({x, y, z});

  initial begin
    #1;
    if (x !== 0 || y !== 0 || z !== 0) failed = 1;
    #10;
    if (x !== 0 || y !== 1 || z !== 0) failed = 1;
    #10;
    if (x !== 1 || y !== 0 || z !== 0) failed = 1;

    if (failed)
      $display("FAILED");
    else
      $display("PASSED");
    $finish;
  end

  always @* $display("%b %b %b", x, y, z);

endmodule // vlog25

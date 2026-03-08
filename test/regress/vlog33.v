module vlog33;
  wire [7:0] x;
  reg        reset = 1;
  reg [7:0]  y = 0;

  assign x = reset ? 0 : (x < y ? x + 1 : x);

  initial begin
    #1;
    reset = 0;
    #1;
    y = 5;
    #0;
    if (x !== 5) begin
      $display("FAILED (1) -- %d", x);
      $finish;
    end
    y = 200;
    #0;
    if (x !== 200) begin
      $display("FAILED (2) -- %d", x);
      $finish;
    end
    $display("PASSED");
  end

endmodule // vlog33

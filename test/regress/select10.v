module select10;
  reg [7:0] x;
  wire y;
  wire [1:0] z;

  assign y = x[8];
  assign z = x[8:7];

  initial begin
    x = 8'h00;
    #1;

    if (y !== 1'bx || z !== 2'bx0) begin
      $display("FAILED (1) -- %b %b", y, z);
      $finish;
    end

    x[6] = 1'b1;
    #1;

    if (y !== 1'bx || z !== 2'bx0) begin
      $display("FAILED (2) -- %b %b", y, z);
      $finish;
    end

    x[7] = 1'b1;
    #1;

    if (y !== 1'bx || z !== 2'bx1) begin
      $display("FAILED (3) -- %b %b", y, z);
      $finish;
    end

    $display("PASSED");
  end
endmodule // select10

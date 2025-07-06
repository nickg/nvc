module select6;
  reg [7:0] array [0:3][1:2];
  reg [7:0] cnt = 0;

  always @(array[2][2][3])
    cnt = cnt + 1;

  initial begin
    array[2][1] = 7;
    array[2][2] = 8;
    array[3][2] = 8;
    array[66][1] = 6;

    if (array[2][1] !== 7 || array[2][2] !== 8) begin
      $display("FAILED (1) -- %x %x %x", array[2][1], array[2][2], array[0][1]);
      $finish;
    end

    array[3][2] = 0;

    #1 if (cnt !== 1) begin
      $display("FAILED (2) -- %d", cnt);
      $finish;
    end

    $display("PASSED");
  end

endmodule // select6

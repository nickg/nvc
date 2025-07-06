module select5;
  reg [7:0] array [0:3];

  initial begin
    array[1] = 42;
    if (array[1] !== 42) begin
      $display("FAILED (1) -- %d !== 42", array[1]);
      $finish;
    end

    array[4] = 55;
    if (array[1] !== 42) begin
      $display("FAILED (2) -- %d !== 42", array[1]);
      $finish;
    end

    array[1][0] = 1;
    if (array[1] !== 43) begin
      $display("FAILED (3) -- %d !== 43", array[1]);
      $finish;
    end else if (array[1][1] !== 1) begin
      $display("FAILED (4) -- %x !== 1", array[1][2]);
      $finish;
    end

    $display("PASSED");
  end
endmodule // select5

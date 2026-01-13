module issue1336;
  localparam p1 = 'h1234;
  wire [3:0] w1 = p1[3:0];
  integer    i1 = 'h5678;
  wire [3:0] w2 = i1[15:12];

  initial begin
    #1;
    $display("w1=%x w2=%x", w1, w2);
    if (w1 == 4'h4 && w2 == 4'h5)
      $display("PASSED");
    else
      $display("FAILED");
  end

endmodule // issue1336

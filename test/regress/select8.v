module select8;
  reg [8:1] arr [1:4];
  reg       fail = 0;

  integer i;
  initial for (i = 1; i < 4; i++) arr[i] = i;

  initial begin
    #0;
    $display("arr[2] : %d", arr[2]);
    if (arr[2] !== 2) fail = 1;

    $display("arr[2][2] : %d", arr[2][2]);
    if (arr[2][2] !== 1) fail = 1;

    $display("arr[2][2:1] : %d", arr[2][2:1]);
    if (arr[2][2:1] !== 2) fail = 1;

    $display("arr[2][1:0] : %b", arr[2][1:0]);
    if (arr[2][1:0] !== 2'bx) fail = 1;

    $display("arr[2][100] : %d", arr[2][100]);
    if (arr[2][100] !== 1'bx) fail = 1;

    if (fail)
      $display("FAILED");
    else
      $display("PASSED");
  end

endmodule // select8

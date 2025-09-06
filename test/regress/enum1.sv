module enum1;
  typedef enum { RED, GREEN, BLUE } t_color;
  t_color x;
  bit failed = 0;
  parameter start = 5;
  enum shortint { A=start, B, C=10, D } y;

  initial begin
    x = RED;
    #1;
    if (x !== RED) failed = 1;
    if (x !== 0) failed = 1;

    x = BLUE;
    #1;
    if (x !== 2) failed = 1;

    y = A;
    #1;
    if (y !== start) failed = 1;

    y = B;
    #1;
    if (y !== 6) failed = 1;

    y = D;
    #1;
    if (y !== 11) failed = 1;

    if (failed)
      $display("FAILED");
    else
      $display("PASSED");
  end

endmodule // enum1

module struct1;
  struct packed {
    logic x;
    logic [3:0] y;
  } s;

  initial begin
    s.x = 1;
    s.y = 4'b1010;

    $displayb(s);

    if (s !== 5'b11010 || s.y != 4'b1010)
      $display("FAILED");
    else
      $display("PASSED");
  end

endmodule // struct1

module pattern1;
  reg [7:0] x;
  struct packed {
    byte f1, f2;
  } y;

  initial begin
    x = '{default:1};
    if (x !== 8'hff)
      $fatal(1, "x: %x != ff", x);
    x = '{5:0,1:0,default:1};
    if (x !== 8'hdd)
      $fatal(1, "x: %x != dd", x);
    y = '{f1:5, f2:6};
    if (y !== 16'h0506)
      $fatal(1, "y: %x != 0506", y);
    y = '{f2: 42, default:9};
    if (y !== 16'h092a)
      $fatal(1, "y: %x != 092a", y);
    $display("PASSED");
  end

endmodule // pattern1

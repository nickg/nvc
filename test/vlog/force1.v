module force1;
  reg x;
  initial begin
    force x = 1;  // OK
    release x;    // OK
  end
endmodule // force1

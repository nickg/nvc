module vlog5;
  wire x, y;

  assign x = 1;
  assign y = 0;
  
  initial begin
    #1;
    $display("%d %d %d %d", x & y, x & 1, y & 1, y & 0);
  end
endmodule // vlog4

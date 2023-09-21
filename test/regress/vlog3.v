module vlog3;
  reg x;
  
  initial begin
    x <= 1;
    $display("x ==> %d", x);
    /*if (x)
      $display("x is true");
    else
      $display("x is false");*/
    #1;
    $display("x ==> %d", x);
    /*if (x)
      $display("x is true");
    else
      $display("x is false");*/
    #1 x <= 0;
    $display("x ==> %d", x);
    #1 x <= 4;
    $display("x ==> %d", x);
    #1;
    $display("x ==> %d", x);
    $finish;
  end
  
endmodule // vlog3

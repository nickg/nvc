module vlog36;
  parameter N = 2;
  wire [7:0] a;
  reg  [1:0] b;
  reg        pass = 1;

  assign a = {{N+1}{b}};

  always @(*)
    $display("a ==> %b", a);

  initial begin
    b = 0;
    #1;
    pass &= (a == 8'b00000000);
    b = 1;
    #1;
    pass &= (a == 8'b00010101);
    b = 2;
    #1;
    pass &= (a == 8'b00101010);
    if (pass)
      $display("PASSED");
    else
      $display("FAILED");
  end

endmodule // vlog36

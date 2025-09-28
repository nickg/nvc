module vlog28;
  wire [7:0] o;
  reg [7:0]  a, b;
  reg        failed = 0;

  assign #5 o = a + b;

  initial begin
    a = 1;
    b = 2;
    #4;
    a = 5;
    b = 6;
    #3;
    a = 9;
    b = 12;
  end

  always @* $display("%t | %d + %d ==> %d", $time, a, b, o);

endmodule // vlog28

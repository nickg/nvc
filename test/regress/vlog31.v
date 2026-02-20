module sub1 (data);
  output [31:0] data;
  reg [31:0]    data = 42;
endmodule // sub

module sub2 (output reg [31:0] data = 66);
endmodule // sub2

module sub3 (data);
  output reg [31:0] data = 77;
endmodule // sub2

module vlog31;
  wire [31:0] x1, x2, x3;

  sub1 u1(x1);
  sub2 u2(x2);
  sub3 u4(x3);

  initial begin
    #1;
    $display("x1=%d x2=%d x3=%d", x1, x2, x3);

    if (x1 === 42 && x2 === 66 && x3 === 77)
      $display("PASSED");
    else
      $display("FAILED");
  end

endmodule // vlog31

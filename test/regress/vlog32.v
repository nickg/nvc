module sub (i, o);
  input [2:0] i;
  output [2:0] o;

  assign o = i;
endmodule // sub

module vlog32;
  wire [2:0] o1, o2;
  reg [2:0]  i1;

  sub u1(3'd1, o1), u2(o1 + i1, o2);

  always @*
    $display("o1=%d o2=%d", o1, o2);

  initial begin
    i1 = 3'd1;
    #1;
    i1 = 3'd2;
    #0;
    if (o1 === 3'd1 && o2 == 3'd3)
      $display("PASSED");
    else
      $display("FAILED");
  end
endmodule // vlog32

// An input port net driven inside the module, or connected to a variable
// with an initial value, is not merged with the variable
module vportdrv1_pull(a, y);
  input a;
  output y;
  wire a;
  assign (pull0, pull1) a = 1'b1;
  assign y = a;
endmodule

module vportdrv1_dly(input [3:0] a, output reg [3:0] q);
  always @(a) q <= a;
endmodule

module vportdrv1;
  reg        r = 1'b0;
  reg  [3:0] addr = 4'd5;
  wire       y1, y2;
  wire [3:0] q;

  vportdrv1_pull u1 (.a(r), .y(y1));
  vportdrv1_pull u2 (.a(r), .y(y2));
  vportdrv1_dly  u3 (.a(addr), .q(q));

  initial begin
    #1;
    if (r !== 1'b0 || y1 !== 1'b0 || y2 !== 1'b0)
      $display("FAILED r=%b y1=%b y2=%b", r, y1, y2);
    else if (q !== 4'd5)
      $display("FAILED q=%b", q);
    else begin
      r = 1'b1;
      addr = 4'd9;
      #1;
      if (y1 !== 1'b1 || y2 !== 1'b1 || q !== 4'd9)
        $display("FAILED y1=%b y2=%b q=%b", y1, y2, q);
      else
        $display("PASSED");
    end
  end
endmodule

module vlog27;
  wire [7:0] o;
  reg [7:0]  x, y;

  assign o = x + y;

  initial begin
    #0;
    x = 0;
    y = 0;
    #1;
    x = 1;
    x = 0;
  end

  always @(o) $display("%t | o ==> %d", $time, o);

endmodule // vlog27

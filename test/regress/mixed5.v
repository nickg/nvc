module sub(o);
  parameter p = 100;
  parameter q;
  localparam width = $clog2(p);
  output [width-1:0] o;
  assign o = q;
endmodule // sub

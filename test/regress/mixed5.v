module sub(o);
  parameter p = 100;
  parameter q;
  localparam width = $clog2((p * 4) / 4) << !(1 && (0 || 1));
  output [width-1:0] o;
  assign o = q;
endmodule // sub

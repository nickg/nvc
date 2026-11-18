// Generate blocks made of a single instance without begin and end
module vgenitem1_sub #(parameter V = 0) (output [3:0] o);
  assign o = V;
endmodule

module vgenitem1;
  parameter SEL = 1;
  wire [3:0] o1, o2;

  generate
    if (SEL == 0)
      vgenitem1_sub #(1) inst (o1);
    else
      vgenitem1_sub #(2) inst (o1);
  endgenerate

  if (SEL)
    vgenitem1_sub #(3) inst (o2);

  initial begin
    #1;
    if (o1 !== 4'd2 || o2 !== 4'd3) $display("FAILED %d %d", o1, o2);
    else $display("PASSED");
  end
endmodule

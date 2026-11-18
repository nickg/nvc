// Procedures waiting for a change of a net see the value its continuous
// assignment gives it at time zero
module vzeroev1;
  wire  w1, w2, w3;
  reg   r = 1'b1;
  reg   d1 = 1'bx, d2 = 1'bx, d3 = 1'bx;

  assign w1 = 1'b1;
  assign w2 = ~r;
  buf (w3, r);

  always @(w1) d1 <= w1;
  always @(w2) d2 <= w2;
  always @(w3) d3 <= w3;

  initial begin
    #1;
    if (d1 !== 1'b1) $display("FAILED d1=%b", d1);
    else if (d2 !== 1'b0) $display("FAILED d2=%b", d2);
    else if (d3 !== 1'b1) $display("FAILED d3=%b", d3);
    else $display("PASSED");
  end
endmodule

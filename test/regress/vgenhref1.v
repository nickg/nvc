module vgenhref1_sub;
  reg [3:0] x = 4'd5;
endmodule

module vgenhref1_bus;
  wire [1:0] bus = 2'b10;
endmodule

module vgenhref1;
  parameter P = 1;
  genvar i;

  if (P) begin : g
    reg [7:0] r = 8'd42;
    reg       clk = 0;
    vgenhref1_sub u ();
    initial #1 if (u.x !== 4'd5) $display("FAILED u.x %d", u.x);
  end

  if (P) begin : h
    reg [7:0] seen = 0;
    always @(posedge g.clk) seen = g.r;    // sibling generate block
  end

  if (!P) begin : n
    initial $display("FAILED %d", missing.r);    // not elaborated
  end

  for (i = 0; i < 3; i = i + 1) begin : gen
    vgenhref1_sub u ();
    initial u.x = i + 1;
  end

  vgenhref1_bus b ();
  wire w = b.bus[1];    // bit select of a hierarchical name

  initial begin
    #2;
    if (g.r !== 8'd42) $display("FAILED g.r %d", g.r);
    if (g.u.x !== 4'd5) $display("FAILED g.u.x %d", g.u.x);
    if ({gen[0].u.x, gen[1].u.x, gen[2].u.x} !== {4'd1, 4'd2, 4'd3})
      $display("FAILED gen %d %d %d", gen[0].u.x, gen[1].u.x, gen[2].u.x);
    g.r = 8'd99;
    g.clk = 1;
    #1;
    if (h.seen !== 8'd99) $display("FAILED h.seen %d", h.seen);
    if (w !== 1'b1 || b.bus[0] !== 1'b0) $display("FAILED bus %b", b.bus);
    $display("PASSED");
  end
endmodule

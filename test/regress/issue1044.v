
module clk_gate_n (
    CLK,
    E,
    GCLK,
    SE
);
input CLK;
input E;
input SE;
output GCLK;

wire CLK;
wire E;
wire SE;
wire GCLK;
reg en1;

always @ (CLK or E or SE) begin
  if (CLK == 1)
    en1 = SE | E;
end

assign GCLK = CLK | !en1;

endmodule // clk_gate_n

module clk_gate_p (
    CLK,
    E,
    GCLK,
    SE
);
input CLK;
input E;
input SE;
output GCLK;

wire CLK;
wire E;
wire SE;
wire GCLK;
reg en1;

always @ (CLK or E or SE) begin
  if (CLK == 0)
    en1 = SE | E;
end

assign GCLK = CLK & en1;

endmodule // clk_gate_p


module prop1;
  reg x, clk;
  reg [7:0] y;

  assert property (!(x && y));   // OK
  assume property (@(posedge clk) x |-> ##[1:2] y);  // OK
  assert property (x |=> ##1 y);  // OK
  Label: cover property (x |-> ##5 y);  // OK
endmodule // prop1

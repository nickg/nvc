module prop1;
  reg x, clk;
  reg [7:0] y;

  assert property (!(x && y));   // OK
  assume property (@(posedge clk) x |-> ##[1:2] y);  // OK
  assert property (x |=> ##1 y);  // OK
  Label: cover property (x |-> ##5 y);  // OK
  assume property (x |-> ##1 $stable(y) [*2]);  // OK
  assert property (x [ + ]) else $error("fail");  // OK
  assert property ((x |-> y));  // OK
  assume property ((($past(x) == 0) && (x == 1)) |-> ##1 y);  // OK
endmodule // prop1

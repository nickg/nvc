module tmp
#(
	parameter REGSIN_N = 16,
	parameter REGSOUT_N = 15
)
(
    input wire [31:0] regmap_i [0:REGSIN_N-1],
    output reg [31:0] regmap_o [0:REGSOUT_N-1]
);

  always @(*) begin
    integer i;
    for (i = 0; i < REGSOUT_N; i++) begin
      if (i < REGSIN_N)
        regmap_o[i] = regmap_i[i] + 1;
      else
        regmap_o[i] = ~0;
    end
  end

endmodule // tmp

module issue1625;
  reg [31:0] regmap_i[0:15];
  wire [31:0] regmap_o[0:14];

  tmp u(regmap_i, regmap_o);

  initial begin
    integer i;
    bit fail;
    fail = 0;
    for (i = 0; i < 15; i++)
      regmap_i[i] = i;
    #1;
    for (i = 0; i < 15; i++) begin
      if (regmap_o[i] !== i + 1) begin
        $display("regmap_o[%d] => %d", i, regmap_o[i]);
        fail = 1;
      end
    end
    if (fail)
      $display("FAILED");
    else
      $display("PASSED");
  end

endmodule // issue1625

module test_core
  #(
    parameter  DATAIN_WIDTH = 16,
    parameter  N = 2,
    parameter  BITGROWTH = 35,
    localparam DATAOUT_FULL_WIDTH = DATAIN_WIDTH + BITGROWTH
  )
  (
    input clk_i,
    input rst_i,
    input  [DATAIN_WIDTH-1:0]     din_i,
    output [DATAOUT_FULL_WIDTH-1:0] dout_o
  );

  reg [DATAOUT_FULL_WIDTH-1:0]  integrator [0:N-1];
  integer i;

  always @(posedge clk_i) begin
    if (rst_i) begin
      for (i = 0; i < N; i = i + 1) begin
        integrator[i] <= {DATAOUT_FULL_WIDTH{1'b0}};
      end
    end else begin
      integrator[0] <= integrator[0] + din_i;
      for (i = 1; i < N; i = i + 1) begin
        integrator[i] <= integrator[i] + integrator[i-1];
      end
    end
  end

  assign dout_o = integrator[N-1];

endmodule

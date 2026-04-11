module test_verilog (
    CLK,
    WRCNT
);

    parameter WMSB_DEPTH = 10;
    input CLK;
    output wire [WMSB_DEPTH:0] WRCNT;

    reg [WMSB_DEPTH:0] CNT;

  initial CNT = 0;

    assign WRCNT = CNT;


   always @(posedge CLK) begin
      CNT <= CNT + 1'b1;
   end

endmodule

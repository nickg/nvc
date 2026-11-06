module FDPE #(
  parameter [0:0] INIT = 1'b1,
  parameter [0:0] IS_C_INVERTED = 1'b0,
  parameter [0:0] IS_D_INVERTED = 1'b0,
  parameter [0:0] IS_PRE_INVERTED = 1'b0
)(
  output Q,

  input C,
  input CE,
  input D,
  input PRE
);

    reg [0:0] IS_C_INVERTED_REG = IS_C_INVERTED;
    reg [0:0] IS_D_INVERTED_REG = IS_D_INVERTED;
    reg [0:0] IS_PRE_INVERTED_REG = IS_PRE_INVERTED;

    tri0 glblGSR = 1'b0;
    wire PRE_in;
    assign PRE_in = (PRE ^ IS_PRE_INVERTED_REG) && (PRE !== 1'bz);

// begin behavioral model
  reg Q_out;

  assign #100 Q = Q_out;

    always @(glblGSR or PRE_in)
      if (glblGSR)
        assign Q_out = INIT;
      else if (PRE_in === 1'b1)
        assign Q_out = 1'b1;
      else if (PRE_in === 1'bx)
        assign Q_out = 1'bx;
      else
        deassign Q_out;

endmodule

`endcelldefine

module deassign1;
  reg C = 1'b0;
  reg CE = 1'b0;
  reg D = 1'b0;
  reg PRE = 1'b0;
  wire Q;

  FDPE dut (
    .Q(Q),
    .C(C),
    .CE(CE),
    .D(D),
    .PRE(PRE)
  );

  initial begin
    #1 PRE = 1'b1;
    #101;
    if (Q !== 1'b1) begin
      $display("FAILED: preset did not drive Q high");
      $fatal;
    end

    // The procedural continuous assignment must override ordinary writes.
    dut.Q_out = 1'b0;
    #101;
    if (Q !== 1'b1) begin
      $display("FAILED: Q_out was not held by assign");
      $fatal;
    end

    // Dropping PRE executes deassign, allowing Q_out to be written again.
    PRE = 1'b0;
    #1 dut.Q_out = 1'b0;
    #101;
    if (Q !== 1'b0) begin
      $display("FAILED: Q_out remained assigned after deassign");
      $fatal;
    end

    $display("PASSED");
    $finish;
  end
endmodule

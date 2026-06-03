// From ivtest/ivltests/pr2138979d.v

module signed6;

  reg sel;
  reg [7:0] a, b;
  reg [5:0] c;
  reg passed;

  wire [15:0] mux_us = sel ? a : $signed(b);
  wire [15:0] mux_su = sel ? $signed(a) : b;
  wire        eql_us = a == $signed(c);
  wire        neq_us = a != $signed(c);

  initial begin
    passed = 1'b1;

    sel = 1'b0;
    a = 8'b00000000;
    b = 8'b10000000;
    #1;

    if (mux_us !== 16'b0000000010000000) begin
      $display("FAILED: unsigned/signed conditional gave %b", mux_us);
      passed = 1'b0;
    end

    sel = 1'b1;
    a = 8'b10000000;
    b = 8'b00000000;
    #1;

    if (mux_su !== 16'b0000000010000000) begin
      $display("FAILED: signed/unsigned conditional gave %b", mux_su);
      passed = 1'b0;
    end

    a = 8'b00111000;
    c = 6'b111000;
    #1;

    if (eql_us !== 1'b1 || neq_us !== 1'b0) begin
      $display("FAILED: unsigned/signed equality gave == %b != %b",
               eql_us, neq_us);
      passed = 1'b0;
    end

    if (passed)
      $display("PASSED");
    else
      $display("FAILED");
  end

endmodule

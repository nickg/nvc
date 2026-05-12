module select18;
  parameter base = -1;

  reg [base+15:base] big = 16'h0123;
  reg [base:base+15] ltl = 16'h3210;

  initial begin
    #1;

    if (big[(base)+:4] !== 4'h3) begin
      $display("FAILED: big negative-base +: select got %h", big[(base)+:4]);
      $finish;
    end

    if (ltl[(base)+:4] !== 4'h3) begin
      $display("FAILED: little negative-base +: select got %h",
               ltl[(base)+:4]);
      $finish;
    end

    if (big[(base+4)+:4] !== 4'h2) begin
      $display("FAILED: big offset negative-base +: select got %h",
               big[(base+4)+:4]);
      $finish;
    end

    if (ltl[(base+4)+:4] !== 4'h2) begin
      $display("FAILED: little offset negative-base +: select got %h",
               ltl[(base+4)+:4]);
      $finish;
    end

    $display("PASSED");
  end
endmodule

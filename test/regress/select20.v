module select20;
  parameter base = -1;

  reg [base+15:base] big;
  reg [base:base+15] ltl;

  initial begin
    big = 16'h0000;
    ltl = 16'h0000;

    big[(base-1)+:4] = 4'b011x;
    ltl[(base-1)+:4] = 4'bx001;
    big[(base+13)+:4] = 4'bx001;
    ltl[(base+13)+:4] = 4'b011x;

    if (big !== 16'h2003) begin
      $display("FAILED: big partial +: L-value write got %h", big);
      $finish;
    end

    if (ltl !== 16'h2003) begin
      $display("FAILED: little partial +: L-value write got %h", ltl);
      $finish;
    end

    big = 16'h0000;
    ltl = 16'h0000;

    big[(base+2)-:4] = 4'b011x;
    ltl[(base+2)-:4] = 4'bx001;
    big[(base+16)-:4] = 4'bx001;
    ltl[(base+16)-:4] = 4'b011x;

    if (big !== 16'h2003) begin
      $display("FAILED: big partial -: L-value write got %h", big);
      $finish;
    end

    if (ltl !== 16'h2003) begin
      $display("FAILED: little partial -: L-value write got %h", ltl);
      $finish;
    end

    $display("PASSED");
  end
endmodule

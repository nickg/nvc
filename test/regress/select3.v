module select3;
  reg [7:0] in, idx;
  wire      out;

  assign out = in[idx];

  initial begin
    in = 'h42;
    idx = 0;
    #1 if (out !== 0) begin
      $display("FAILED (1) -- %d !== 0", out);
      $finish;
    end
    idx = 1;
    #1 if (out !== 1) begin
      $display("FAILED (2) -- %d !== 1", out);
      $finish;
    end
    idx = 100;
    #1 if (out !== 1'bx) begin
      $display("FAILED (3) -- %d !== x", out);
      $finish;
    end
    $display("PASSED");
  end
endmodule // select3

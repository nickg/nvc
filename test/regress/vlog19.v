module vlog19;
  reg [7:0] x, y;
  wire [7:0] out;

  assign out = x > y ? x : y;

  initial begin
    x = 2;
    y = 3;
    #1 if (out !== 3) begin
      $display("FAILED (1) -- %d", out);
      $finish;
    end

    x[7] = 1;
    #1 if (out !== 130) begin
      $display("FAILED (1) -- %d", out);
      $finish;
    end

    $display("PASSED");
  end

endmodule // vlog19

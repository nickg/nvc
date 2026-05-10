module binary8;

  reg pass;
  reg [3:0] a, b, r;

  initial begin
    pass = 1'b1;

    a = 4'b110x;
    b = 4'b1011;

    r = a ~& b;
    if (r !== 4'b011x) begin
      $display("FAILED: binary nand gave %b", r);
      pass = 1'b0;
    end

    r = a ~| b;
    if (r !== 4'b0000) begin
      $display("FAILED: binary nor gave %b", r);
      pass = 1'b0;
    end

    a = 4'b010x;
    b = 4'b0010;

    r = a ~| b;
    if (r !== 4'b100x) begin
      $display("FAILED: binary nor x gave %b", r);
      pass = 1'b0;
    end

    a = 4'b0101;
    b = 4'b0011;

    r = a ~& b;
    if (r !== 4'b1110) begin
      $display("FAILED: binary nand known gave %b", r);
      pass = 1'b0;
    end

    r = a ~| b;
    if (r !== 4'b1000) begin
      $display("FAILED: binary nor known gave %b", r);
      pass = 1'b0;
    end

    if (pass)
      $display("PASSED");
    else
      $display("FAILED");
  end

endmodule

module binary1;

  reg [7:0] n1, n2, n3, n4, n5, n6;
  reg [199:0] w1, w2, w3, w4;

  initial begin
    n1 = 42;
    #1;
    n2 = n1 ^ 66;
    n3 = n1 & 11;
    n4 = n2 ~^ 98;
    $display("%x %x %x %x %x", n2, n3, n4, n5, n6);
    if (n2 !== 8'h68 || n3 !== 8'h0a || n4 !== 8'hf5) begin
      $display("FAILED");
      $finish;
    end

    w1 = 200'd65225 << 100;
    #1;
    w2 = w1 * 5;
    w3 = w1 << 50;
    $display("%x %x %x", w1, w2, w3);
    if (w1 !== 200'hfec90000000000000000000000000 ||
        w2 !== 200'h4f9ed0000000000000000000000000 ||
        w3 !== 200'h3fb240000000000000000000000000000000000000) begin
      $display("FAILED");
      $finish;
    end

    $display("PASSED");
  end

endmodule // binary1

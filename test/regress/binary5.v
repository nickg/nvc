module binary5;
  reg signed [15:0] s1, s2;
  reg signed [16:0] s3;

  initial begin
    //
    // Test with non-blocking assignment
    //

    s1 = 16'h2759;
    s2 = 16'h65f1;
    #1;

    s3 <= s1 - s2;
    #1;
    if (s3 !== 17'h1c168) begin
      $display("FAILED -- %x - %x ==> %x", s1, s2, s3);
      $finish;
    end

    s1 = 16'h639e;
    s2 = 16'h81a3;
    #1;

    s3 <= s1 - s2;
    #1;
    if (s3 !== 17'he1fb) begin
      $display("FAILED -- %x - %x ==> %x", s1, s2, s3);
      $finish;
    end

    //
    // Test with blocking assignment
    //

    s1 = 16'h2759;
    s2 = 16'h65f1;
    #1;

    s3 = s1 - s2;
    #1;
    if (s3 !== 17'h1c168) begin
      $display("FAILED -- %x - %x ==> %x", s1, s2, s3);
      $finish;
    end

    s1 = 16'h639e;
    s2 = 16'h81a3;
    #1;

    s3 = s1 - s2;
    #1;
    if (s3 !== 17'he1fb) begin
      $display("FAILED -- %x - %x ==> %x", s1, s2, s3);
      $finish;
    end

    $display("PASSED");
  end

endmodule // binary5

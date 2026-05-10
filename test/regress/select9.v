module select9;
  reg [7:0] r8_1, r8_2, r8_3, r8_4;
  integer   idx;

  initial begin
    r8_1[8:1] = 8'hff;
    r8_2[1:-5] = 8'h00;
    $displayb(r8_1,,r8_2);

    if (r8_1 !== 8'b1111111x || r8_2 !== 8'bxxxxxx00) begin
      $display("FAILED");
      $finish;
    end

    idx = 10;
    #1;

    r8_1[idx-:8] = 8'h00;
    r8_2[idx-5-:8] = 8'hff;
    $displayb(r8_1,,r8_2);

    if (r8_1 !== 8'b0000011x || r8_2 !== 8'bxx111111) begin
      $display("FAILED");
      $finish;
    end

    r8_1 = 8'b10100101;
    r8_2 = 8'b11010010;
    r8_3 = r8_1[8:1];
    r8_4 = r8_2[1:-6];
    $displayb(r8_3,,r8_4);

    if (r8_3 !== 8'bx1010010 || r8_4 !== 8'b10xxxxxx) begin
      $display("FAILED");
      $finish;
    end

    idx = 10;
    #1;

    r8_3 = r8_1[idx-:8];
    r8_4 = r8_2[idx-5-:8];
    $displayb(r8_3,,r8_4);

    if (r8_3 !== 8'bxxx10100 || r8_4 !== 8'b010010xx) begin
      $display("FAILED");
      $finish;
    end

    $display("PASSED");
  end

endmodule // select9

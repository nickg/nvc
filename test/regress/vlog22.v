module vlog22;
  wire [7:0] foo, bar;

  if (1 < 2) begin : FOO
    reg [7:0] x;
    assign foo = x;
    initial x = 0;
    always begin
      #1;
      x = x + 1;
    end
  end

  if (5 != 2) begin
    reg [7:0] x;
    assign bar = x;
    initial x = 0;
    always begin
      #1;
      x = x - 1;
    end
  end

  initial begin
    #10;
    $display("%x %x", foo, bar);
    if (foo === 8'h09 && bar === 8'hf7)
      $display("PASSED");
    else
      $display("FAILED");
    $finish;
  end

endmodule // vlog22
